#!/usr/bin/env python3
"""Export an explicitly bounded supplement from the retained presentation archive."""

import argparse
import csv
import hashlib
import json
import math
import re
import shutil
from collections import defaultdict
from pathlib import Path


ROOT = Path(__file__).resolve().parent
FORBIDDEN_COLUMNS = {
    "case_label", "case_id", "row_id", "respondent_id", "student_id", "school_id",
    "prediction_id", "case_imputed_features", "assessment_rows", "training_rows",
}
EXTRA_PISA_TABLES = (
    "pisa_all_pv_missingness_indicator_comparison.csv",
    "pisa_all_pv_missingness_indicator_fold_performance.csv",
    "pisa_all_pv_missingness_indicator_pooled_performance.csv",
    "pisa_all_pv_calibration_summary.csv",
    "pisa_all_pv_calibration_descriptive_summary.csv",
    "pisa_all_pv_calibration_curve.csv",
)


def read_csv(path):
    with path.open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream))


def write_csv(path, rows):
    if not rows:
        raise ValueError(f"No records supplied for {path}")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def quantile_type8(values, probability):
    values = sorted(values)
    h = (len(values) + 1 / 3) * probability + 1 / 3
    if h <= 1:
        return values[0]
    if h >= len(values):
        return values[-1]
    lower = math.floor(h)
    fraction = h - lower
    return values[lower - 1] * (1 - fraction) + values[lower] * fraction


def summarize_cases(rows, group, metrics):
    groups = defaultdict(list)
    for row in rows:
        groups[float(row[group])].append(row)
    summaries = []
    for value, cases in sorted(groups.items()):
        if len(cases) < 20:
            raise ValueError("Do not export a local-audit display group with fewer than 20 cases.")
        summary = {group: int(value) if value.is_integer() else value, "n_cases": len(cases)}
        for metric in metrics:
            values = [float(case[f"median_{metric}"]) for case in cases]
            if not all(math.isfinite(value) for value in values):
                raise ValueError("Nonfinite within-case medians cannot be exported.")
            for label, probability in (("median", 0.5), ("q25", 0.25), ("q75", 0.75)):
                summary[f"{label}_{metric}"] = quantile_type8(values, probability)
        summaries.append(summary)
    return summaries


def selected_figure(figure_id):
    return any(part in figure_id for part in ("calibration", "ale_", "ice_", "subgroup", "multiplicity"))


def export(archive):
    archive = archive.resolve(strict=True)
    if archive.name != "paper_bw":
        raise ValueError("Expected the retained paper_bw presentation archive.")
    registered = {row["pdf_path"]: row for row in read_csv(archive / "FIGURE_MANIFEST.csv")}
    current_manifest = ROOT / "FIGURE_MANIFEST.csv"
    current = {row["figure_id"]: row for row in read_csv(current_manifest)} if current_manifest.exists() else {}
    source_tables = []
    figures = []
    compatibility = []
    semantic_path = ROOT.parent / "final_docs" / "ALE_OBJECT_UNCERTAINTY_METADATA.csv"
    semantics = {row["figure_group_id"]: row for row in read_csv(semantic_path)}

    for study in ("shild", "pisa"):
        metadata = read_csv(archive / study / "provenance" / "figure_latex_metadata.csv")
        metadata = [row for row in metadata if selected_figure(row["figure_id"])]
        if len(metadata) != 15:
            raise ValueError(f"Expected exactly 15 selected {study} components.")
        names = set()
        for original in metadata:
            row = dict(original)
            previous = current.get(row["figure_id"], {})
            keep_revised = previous.get("generator_script") == "analysis/render_supplement_figures.R"
            for name in re.findall(r"tables/([A-Za-z0-9_]+\.csv)", row["source_tables"]):
                names.add(name)
            for suffix in ("pdf", "png"):
                rel = Path(study) / "figures" / f"{row['figure_id']}.{suffix}"
                source = archive / rel
                registered_row = registered[str(rel.with_suffix(".pdf"))]
                if sha256(source) != registered_row[f"{suffix}_sha256"]:
                    raise ValueError(f"Retained {suffix} bytes differ from their original manifest: {rel}")
                target = ROOT / rel
                target.parent.mkdir(parents=True, exist_ok=True)
                if keep_revised:
                    if not target.exists() or sha256(target) != previous[f"{suffix}_sha256"]:
                        raise ValueError(f"The revised public figure changed unexpectedly: {rel}")
                else:
                    shutil.copy2(source, target)
                row[f"{suffix}_path"] = str(rel)
                row[f"{suffix}_sha256"] = sha256(target)
                row[f"retained_{suffix}_sha256"] = sha256(source)
            row["source_tables"] = " | ".join(
                f"{study}/tables/{name}" for name in sorted(re.findall(r"tables/([A-Za-z0-9_]+\.csv)",
                                                                         row["source_tables"]))
            )
            row["source_archive"] = "paper_artifacts/current/paper_bw"
            row["derivation_scope"] = "Existing aggregate figures; unchanged PDF and PNG bytes; no empirical rerun"
            if keep_revised:
                for field in ("generator_script", "generator_function", "derivation_scope", "latex_assembly_note"):
                    row[field] = previous[field]
            semantic = semantics.get(row["figure_group_id"])
            if semantic:
                row["estimand_or_scope"] = " | ".join(semantic[field] for field in (
                    "selected_learner", "fit_scope", "plausible_value_scope", "analytic_distribution"
                ))
                row["uncertainty_semantics"] = semantic["uncertainty_semantics"]
            figures.append(row)
        if study == "pisa":
            names.update(EXTRA_PISA_TABLES)
        for name in sorted(names):
            source = archive / study / "tables" / name
            rows = read_csv(source)
            if not rows or FORBIDDEN_COLUMNS.intersection(rows[0]):
                raise ValueError(f"Unsafe or empty table selected: {name}")
            target = ROOT / study / "tables" / name
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
            source_tables.append({
                "study": study.upper(), "path": str(target.relative_to(ROOT)), "n_rows": len(rows),
                "sha256": sha256(source), "source_path": str(source.relative_to(archive)),
                "release_scope": "Aggregate diagnostic output; no respondent, student, school, or case identifiers",
            })
            canonical = ROOT.parent / "baseline_evidence" / "STUDY_RESULTS" / study.upper() / "tables" / name
            if canonical.exists():
                identical = canonical.read_bytes() == source.read_bytes()
                if not identical:
                    raise ValueError(f"Supplement differs from canonical retained evidence: {name}")
                compatibility.append({"table": name, "check": "canonical baseline byte identity", "passed": True})

    compare = read_csv(ROOT / "pisa/tables/pisa_all_pv_missingness_indicator_comparison.csv")
    rmse = [float(row["difference_missing_indicator_minus_primary"])
            for row in compare if row["measure_id"] == "regr.rmse"]
    if len(rmse) != 10 or not -0.708 < min(rmse) < -0.706 or not -0.628 < max(rmse) < -0.626:
        raise ValueError("Missing-indicator sensitivity does not match the manuscript's retained range.")
    compatibility.append({"table": "pisa_all_pv_missingness_indicator_comparison.csv",
                          "check": "all ten PV RMSE improvements match manuscript 0.627-0.707", "passed": True})

    input_sources = []
    for source_name, group, metrics, output_name, expected in (
        ("shild_g4_local_fidelity_cases.csv", "prediction_score_decile", ("weighted_r2", "weighted_rmse"),
         "shild_g4_decile_plot_summary.csv", 300),
        ("shild_g4_local_fidelity_bandwidth_cases.csv", "kernel_width", ("weighted_rmse",),
         "shild_g4_bandwidth_plot_summary.csv", 1200),
    ):
        source = archive / "evidence/tables" / source_name
        rows = read_csv(source)
        if len(rows) != expected or any(int(row["n_perturbation_replicates"]) != 20 for row in rows):
            raise ValueError("The private display input does not match the registered case-replicate design.")
        write_csv(ROOT / "plot_data" / output_name, summarize_cases(rows, group, metrics))
        input_sources.append({
            "input_name": source_name, "sha256": sha256(source), "n_rows": len(rows),
            "distributed": False, "derived_public_file": f"plot_data/{output_name}",
            "derivation": "Median and type-8 25th/75th percentiles across within-case medians over 20 replicates",
            "privacy_scope": "Input remains private; only decile or bandwidth aggregate summaries are distributed",
        })
    shutil.copy2(semantic_path, ROOT / "ALE_OBJECT_UNCERTAINTY_METADATA.csv")
    write_csv(ROOT / "FIGURE_MANIFEST.csv", figures)
    write_csv(ROOT / "SOURCE_TABLE_MANIFEST.csv", source_tables)
    write_csv(ROOT / "CANONICAL_COMPATIBILITY.csv", compatibility)
    write_csv(ROOT / "PRIVATE_INPUT_PROVENANCE.csv", input_sources)
    refresh_checksums()
    verify()


def refresh_checksums():
    for row in read_csv(ROOT / "FIGURE_MANIFEST.csv"):
        for suffix in ("pdf", "png"):
            if sha256(ROOT / row[f"{suffix}_path"]) != row[f"{suffix}_sha256"]:
                raise ValueError("Figure hashes must be reconciled in the manifest before refreshing checksums.")
    payloads = sorted(path for path in ROOT.rglob("*") if path.is_file() and path.name != "SHA256SUMS")
    (ROOT / "SHA256SUMS").write_text(
        "".join(f"{sha256(path)}  {path.relative_to(ROOT)}\n" for path in payloads), encoding="utf-8"
    )


def verify():
    for line in (ROOT / "SHA256SUMS").read_text(encoding="utf-8").splitlines():
        expected, relative = line.split("  ", 1)
        path = ROOT / relative
        if not path.is_file() or sha256(path) != expected:
            raise ValueError(f"Missing or changed supplement file: {relative}")
    figures = read_csv(ROOT / "FIGURE_MANIFEST.csv")
    if len(figures) != 30 or len({row["figure_id"] for row in figures}) != 30:
        raise ValueError("The supplement must contain exactly 30 unique figure components.")
    for row in figures:
        for source in row["source_tables"].split(" | "):
            if not (ROOT / source).is_file():
                raise ValueError(f"Missing public plotting table: {source}")
    for path in list(ROOT.glob("*/tables/*.csv")) + list(ROOT.glob("plot_data/*.csv")):
        rows = read_csv(path)
        if not rows or FORBIDDEN_COLUMNS.intersection(rows[0]):
            raise ValueError(f"Unexpected individual-level identifier in {path}")
    print(json.dumps({"figure_components": len(figures), "source_tables": len(read_csv(
        ROOT / "SOURCE_TABLE_MANIFEST.csv")), "safe_local_audit_plot_tables": 2,
        "empirical_analyses_rerun": False, "verification": "passed"}, indent=2))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--archive", type=Path, help="Local retained paper_bw archive; required only for release assembly")
    parser.add_argument("--verify", action="store_true", help="Verify the distributed public supplement without private inputs")
    parser.add_argument("--refresh-checksums", action="store_true", help="Refresh delivery checksums after a verified public re-render")
    arguments = parser.parse_args()
    if arguments.verify:
        verify()
    elif arguments.refresh_checksums:
        refresh_checksums()
        verify()
    elif arguments.archive:
        export(arguments.archive)
    else:
        parser.error("Supply --verify or an explicit --archive directory.")
