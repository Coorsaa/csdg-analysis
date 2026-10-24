#!/usr/bin/env python3
"""Assemble the computational supplement from the public figure components; never estimate curves.

Writes publication.tex and PUBLICATION_INDEX.csv to this folder, builds the PDF with XeLaTeX in the output
directory, copies it to computational_supplement.pdf in this folder, and refreshes and verifies SHA256SUMS.
"""
import argparse
import csv
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent
ARTICLE_TITLE = ("From Interpretation Claims to Diagnostic Evidence: A Practical Framework for Interpretable "
                 "Machine Learning in Psychological Research")
REPOSITORY_URL = "https://github.com/coorsaa/csdg-analysis"
PDF_NAME = "computational_supplement.pdf"
# Fixed PDF creation date so that rebuilding with the same TeX installation reproduces the same bytes.
SOURCE_DATE_EPOCH = "1790553600"  # 2026-09-28T00:00:00Z
FIGURE_PREFIX = "C"  # distinguishes these figures from the figures of the methods supplement (S1, S2, ...)
VARIABLES = {
    "age": ("Age", "years"),
    "e1": ("Height (E1)", "centimeters"),
    "e2": ("Weight (E2)", "kilograms"),
    "m6": ("Spending on medicine and nutritional supplements in the past year (M6)", "Danish kroner"),
    "effort2": ("Hypothetical test effort (EFFORT2)",
                "reported effort-rating points, not actual effort (EFFORT1)"),
    "pared": ("Highest parental education (PARED)", "years of schooling"),
    "hisei": ("Highest parental occupational status (HISEI)", "occupational-status index points"),
    "joyread": ("Joy/like reading (JOYREAD)", "standardized weighted-likelihood index units"),
}


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def tex(text):
    replacements = {"\\": r"\textbackslash{}", "&": r"\&", "%": r"\%", "_": r"\_",
                    "#": r"\#", "$": r"\$", "{": r"\{", "}": r"\}"}
    return "".join(replacements.get(char, char) for char in str(text)).replace("\n", " ")


def caption(row):
    ident = row["figure_id"]
    study = row["study"]
    heading = row["title"] or row["group_title"]
    heading = re.sub(r"ROC AUC audit$", "ROC AUC comparison", heading)
    heading = heading.replace("focal-model ", "XGBoost ").replace("focal ", "")
    part = row["panel_id"]
    subgroup = {"audit_age_group": "age group", "audit_gender": "gender",
                "audit_immigration": "immigration background"}.get(part)
    if subgroup:
        heading = f"{heading}, by {subgroup}"
    detail = row["subtitle"] or row["group_subtitle"]
    detail = detail.replace("; Whiskers", "; whiskers").replace("focal XGBoost", "XGBoost")
    detail = detail.replace("0.5–1.0 AUC", ".5–1.0 AUC").replace("0.5-1.0 AUC", ".5-1.0 AUC")
    if "rich_ale_1d" in ident or ("rich_ice_quantiles" in ident and part != "legend"):
        variable, units = VARIABLES[part]
        heading = f"{study}: {'ALE' if 'ale_1d' in ident else 'aggregate ICE'} for {variable}"
        detail = f"Horizontal axis: {variable}, measured in {units}. " + detail
        if "ice" in ident:
            detail += (" Quantiles summarize the 30 reference-case prediction curves at each queried value; "
                       "they are not confidence intervals. Queries may leave empirical support and are not causal effects "
                       "or feasible changes for individuals.")
        else:
            detail += (" The curve is centered accumulated local prediction differences from the XGBoost fit to the full "
                       "analytic sample over supported empirical intervals. Cells below 20 reference observations "
                       "are suppressed. Bands use 500 complete fixed-pipeline refits on common boundaries; "
                       "they omit model-selection variation and are not simultaneous.")
    if "pisa" in ident:
        detail += (" Scope: first reading plausible value in the unweighted 48,000-student equal-country sample "
                   "across 32 observed countries, not design-weighted population inference.")
    if ident == "pisa_rich_ale_2d":
        detail += (" The variables are the standardized JOYREAD index and hypothetical test effort EFFORT2 "
                   "(rating points), not actual effort EFFORT1. First-order effects are removed.")
    if ident == "shild_rich_ale_2d":
        detail += " Age is measured in years and height in centimeters; first-order effects are removed."
    if "multiplicity" in ident:
        detail += (" Range means the largest minus smallest accepted-model OOF prediction for the same row. "
                   "The cumulative step curve summarizes fixed-width bins; separately labeled quantile anchors "
                   "retain exact aggregate values. This is not a ROC curve or an exact row-level ECDF.")
    return heading, detail


def assemble(output):
    if output.resolve() == ROOT or ROOT in output.resolve().parents:
        raise ValueError("Choose a build directory outside supplement/, for example build/supplement.")
    output.mkdir(parents=True, exist_ok=True)
    rows = list(csv.DictReader((ROOT / "FIGURE_MANIFEST.csv").open()))
    assert len(rows) == 30 and len({r["figure_id"] for r in rows}) == 30
    for row in rows:
        assert sha(ROOT / row["pdf_path"]) == row["pdf_sha256"], row["figure_id"]
    legends = {row["study"]: row for row in rows if row["panel_id"] == "shared_legend"}
    included_legends = set()
    lines = [r"% !TeX program = xelatex", r"\documentclass[11pt]{article}",
             r"\usepackage[margin=0.8in]{geometry}", r"\usepackage{fontspec}",
             r"\setmainfont{Times New Roman}", r"\setsansfont{Arial}",
             r"\usepackage{graphicx,caption,hyperref,xurl}",
             r"\hypersetup{hidelinks,pdftitle={Computational Supplement},pdfsubject={" + ARTICLE_TITLE + "}}",
             r"\captionsetup{font=small,labelfont=bf,justification=raggedright,singlelinecheck=false}",
             r"\renewcommand{\thefigure}{" + FIGURE_PREFIX + r"\arabic{figure}}", r"\setlength{\parindent}{0pt}",
             r"\setlength{\emergencystretch}{2em}", r"\begin{document}",
             r"\section*{Computational Supplement}",
             r"\textbf{" + ARTICLE_TITLE + r"}\par",
             r"\medskip",
             "This supplement contains the diagnostic figures of the SHILD and PISA applications on calibration, "
             "subgroup comparisons, one- and two-dimensional ALE, aggregate ICE, and prediction multiplicity. "
             "It assembles 30 monochrome components as 28 captioned figures, numbered " + FIGURE_PREFIX + "1--"
             + FIGURE_PREFIX + "28 to distinguish them from the figures of the methods supplement (S1, S2, ...). "
             "Two shared ICE legends are shown with their first corresponding plot. Decimal labels follow APA "
             "conventions, and captions state the variable, scale, and variation target." + r"\par",
             r"\medskip",
             "SHILD describes the unweighted two-extremes life-satisfaction analytic sample; PISA describes "
             "the unweighted equal-country reading sample. These fitted-model diagnostics do not establish causal "
             "effects, transport, or adequacy for individual decisions. Pointwise bootstrap intervals, across-case "
             "ICE quantiles, and descriptive model ranges answer different questions." + r"\par",
             r"\medskip",
             "The figure components, their aggregate source tables, and this document are in the folder "
             r"\texttt{supplement/} of the analysis repository (\url{" + REPOSITORY_URL + "}). "
             r"\texttt{PUBLICATION\_INDEX.csv} maps every component to its figure, page, caption, generator, "
             "public source tables, and checksums; "
             r"\path{ALE_OBJECT_UNCERTAINTY_METADATA.csv} describes the uncertainty objects in full."]
    index = []
    for number, row in enumerate((r for r in rows if r["panel_id"] != "shared_legend"), 1):
        title, text = caption(row)
        lines.extend([r"\clearpage", r"\begin{figure}[p]", r"\centering",
                      rf"\includegraphics[width=.98\linewidth,height=5.8in,keepaspectratio]{{{row['pdf_path']}}}"])
        legend = None
        if "ice_quantiles" in row["figure_id"] and row["study"] not in included_legends:
            legend = legends[row["study"]]
            lines.extend([r"\par\smallskip", rf"\includegraphics[width=.98\linewidth]{{{legend['pdf_path']}}}"])
            included_legends.add(row["study"])
        lines.extend([rf"\caption{{\textbf{{{tex(title)}.}} {tex(text)}}}",
                      rf"\label{{supp:{row['figure_id']}}}", r"\end{figure}"])
        for component in ([row, legend] if legend else [row]):
            index.append({"component_id": component["figure_id"], "study": row["study"],
                          "figure": f"{FIGURE_PREFIX}{number}", "supplement_page": number + 1,
                          "component_path": component["pdf_path"], "caption_heading": title,
                          "caption": text, "source_tables": component["source_tables"],
                          "generator_script": component["generator_script"],
                          "retained_pdf_sha256": component["retained_pdf_sha256"],
                          "manifest_pdf_sha256": component["pdf_sha256"],
                          "assembled_pdf_sha256": sha(ROOT / component["pdf_path"]),
                          "role": "shared legend" if component == legend else "diagnostic figure"})
    lines.extend([r"\clearpage", r"\end{document}"])
    # A content-derived PDF trailer ID keeps the PDF bytes independent of the build directory.
    trailer_id = hashlib.md5("\n".join(lines).encode("utf-8")).hexdigest()
    lines.insert(lines.index(r"\begin{document}") + 1, rf"\special{{pdf:trailerid [<{trailer_id}> <{trailer_id}>]}}")
    source = ROOT / "publication.tex"
    source.write_text("\n".join(lines) + "\n")
    command = ["latexmk", "-xelatex", "-interaction=nonstopmode", "-halt-on-error",
               f"-outdir={output.resolve()}", source.name]
    env = dict(os.environ)
    env["PATH"] = "/Library/TeX/texbin:" + env.get("PATH", "")
    env["SOURCE_DATE_EPOCH"] = SOURCE_DATE_EPOCH
    result = subprocess.run(command, cwd=ROOT, env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (output / "assembly_command.log").write_text(" ".join(command) + "\n" + result.stdout)
    if result.returncode:
        raise RuntimeError(result.stdout[-6000:])
    aux = (output / "publication.aux").read_text()
    pages = dict(re.findall(r"\\newlabel\{supp:([^}]+)\}\{\{[^}]*\}\{(\d+)\}", aux))
    for item in index:
        key = item["component_id"] if item["role"] != "shared legend" else next(
            r["component_id"] for r in index if r["figure"] == item["figure"] and r["role"] == "diagnostic figure")
        item["supplement_page"] = int(pages[key])
    with (ROOT / "PUBLICATION_INDEX.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(index[0]))
        writer.writeheader()
        writer.writerows(index)
    shutil.copyfile(output / "publication.pdf", ROOT / PDF_NAME)
    sys.dont_write_bytecode = True
    import build_supplement
    build_supplement.refresh_checksums()
    build_supplement.verify()
    print(json.dumps({"components": len(index), "figures": len(pages), "pdf": str(ROOT / PDF_NAME),
                      "empirical_execution": False}))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True, help="Build directory for XeLaTeX (outside supplement/)")
    assemble(parser.parse_args().output)
