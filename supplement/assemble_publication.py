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
ARTICLE_TITLE = ("Which Conclusions Does an Explanation Support? Claim-Scoped Diagnostic Gates for Interpretable "
                 "Machine Learning in Psychology")
REPOSITORY_URL = "https://github.com/coorsaa/csdg-analysis"
PDF_NAME = "computational_supplement.pdf"
# Fixed PDF creation date so that rebuilding with the same TeX installation reproduces the same bytes.
SOURCE_DATE_EPOCH = "1790553600"  # 2026-09-28T00:00:00Z
FIGURE_PREFIX = "CS"  # distinguishes these figures from the figures of the online supplement (S1, S2, ...)
AUTHORS = "Stefan Coors, Clara S. Vetter, Jonas Hauck, Bernd Bischl, and Sven Hilbert"
# Wording of the article for terms that the figure manifest words differently.
TERMS = [("log-loss-near-equivalent models", "similarly accurate learners"), ("near-equivalent models", "similarly accurate learners"),
         ("prediction multiplicity", "differences in individual predictions"), ("out-of-fold", "held-out"), ("OOF", "held-out"),
         ("ROC AUC", "AUC"), ("not CIs", "not confidence intervals"), ("p50, p90, and p95", "median, 90th, and 95th percentile"),
         ("PISA PV1 ", "PISA, first plausible value: "), ("conditional on PV1", "conditional on the first plausible value"),
         ("64-84", "64\u201384")]
VARIABLES = {
    "age": ("Age", "years"),
    "e1": ("Height (E1)", "centimeters"),
    "e2": ("Weight (E2)", "kilograms"),
    "m6": ("Spending on medicine and nutritional supplements in the past year (M6)", "Danish kroner"),
    "effort2": ("Hypothetical test effort (EFFORT2)",
                "reported effort-rating points, not actual effort (EFFORT1)"),
    "pared": ("Highest parental education (PARED)", "years of schooling"),
    "hisei": ("Highest parental occupational status (HISEI)", "occupational-status index points"),
    "joyread": ("Enjoyment of reading (JOYREAD)", "standardized weighted-likelihood index units"),
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
    heading = re.sub(r"ROC AUC audit$", "AUC comparison", heading)
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
            detail += (" Quantiles summarize the 30 reference-case prediction curves at each queried value. Queries may "
                       "leave empirical support and are not causal effects or feasible changes for individuals.")
        else:
            detail += (" The curve is centered accumulated local prediction differences from the XGBoost fit to the full "
                       "analytic sample over supported empirical intervals. Cells below 20 reference observations "
                       "are suppressed. Bands use 500 complete fixed-pipeline refits on common boundaries; "
                       "they omit model-selection variation.")
    if ident == "pisa_rich_ale_2d":
        detail += (" The variables are the standardized JOYREAD index and hypothetical test effort EFFORT2 "
                   "(rating points), not actual effort EFFORT1." + ("" if "first-order" in detail.lower() else " First-order effects are removed."))
    if ident == "shild_rich_ale_2d":
        detail += " Age is measured in years and height in centimeters." + ("" if "first-order" in detail.lower() else " First-order effects are removed.")
    if "multiplicity" in ident:
        detail += (" Range means the largest minus the smallest held-out prediction of the similarly accurate learners "
                   "for the same respondent or student. The cumulative step curve summarizes fixed-width bins; "
                   "separately labeled quantile anchors retain exact aggregate values.")
    for old, new in TERMS:
        heading, detail = heading.replace(old, new), detail.replace(old, new)
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
             r"\captionsetup{font=small,labelfont=bf,textfont=it,labelsep=newline,justification=raggedright,singlelinecheck=false}",
             r"\renewcommand{\thefigure}{" + FIGURE_PREFIX + r"\arabic{figure}}", r"\setlength{\parindent}{0pt}",
             r"\setlength{\emergencystretch}{2em}", r"\begin{document}",
             r"\section*{Computational Supplement}",
             r"\textbf{" + ARTICLE_TITLE + r"}\par", AUTHORS + r"\par",
             r"\medskip",
             "This supplement shows the diagnostic displays of the applications to the Survey of Health, Impairment "
             "and Living Conditions in Denmark (SHILD) and the Programme for International Student Assessment (PISA) "
             "that Supplement F of the online supplement lists: calibration, subgroup comparisons, accumulated local "
             "effects (ALE), individual conditional expectation (ICE) curves, interaction surfaces, and differences in "
             "individual predictions across similarly accurate learners. No claim of the article depends on them. The "
             "figures are numbered " + FIGURE_PREFIX + "1 to " + FIGURE_PREFIX + "28 to distinguish them from the "
             "figures of the online supplement (S1 to S4)." + r"\par",
             r"\medskip",
             "The SHILD displays describe the unweighted analytic sample of respondents who reported being satisfied "
             "or dissatisfied with their lives. The PISA displays describe the first plausible value of reading "
             "proficiency in the unweighted sample of 48,000 students from 32 countries, without design weights. None "
             "of the displays establishes causal effects, transport to other settings, or adequacy for decisions about "
             "individuals." + r"\par",
             r"\medskip",
             r"Some labels inside the figures word things differently from the article: ``discontent'' denotes "
             r"dissatisfaction with life, ``OOF'' (out of fold) denotes held-out predictions, ``PV1'' the first "
             r"plausible value, ``p50'' the median, and ``ROC AUC'' the AUC.\par",
             r"\medskip",
             "Abbreviations: ALE = accumulated local effects; AUC = area under the receiver operating characteristic "
             "curve; ICE = individual conditional expectation; PISA = Programme for International Student Assessment; "
             "RMSE = root mean squared error; SHILD = Survey of Health, Impairment and Living Conditions in Denmark; "
             "WLE = weighted likelihood estimate." + r"\par",
             r"\medskip",
             "The figure components and their aggregate source tables are in the analysis repository "
             r"(\url{" + REPOSITORY_URL + "})."]
    index = []
    for number, row in enumerate((r for r in rows if r["panel_id"] != "shared_legend"), 1):
        title, text = caption(row)
        lines.extend([r"\clearpage", r"\begin{figure}[p]", rf"\caption{{{tex(title)}}}",
                      rf"\label{{supp:{row['figure_id']}}}", r"\centering",
                      rf"\includegraphics[width=.98\linewidth,height=5.6in,keepaspectratio]{{{row['pdf_path']}}}"])
        legend = None
        if "ice_quantiles" in row["figure_id"] and row["study"] not in included_legends:
            legend = legends[row["study"]]
            lines.extend([r"\par\smallskip", rf"\includegraphics[width=.98\linewidth]{{{legend['pdf_path']}}}"])
            included_legends.add(row["study"])
        lines.extend([r"\par\smallskip{\raggedright\small\textit{Note.} " + tex(text) + r"\par}", r"\end{figure}"])
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
