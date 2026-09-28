"""Check the eight targeted PDFs without modifying their scientific contents."""

import hashlib
import json
import subprocess
import xml.etree.ElementTree as ET
from pathlib import Path


def verify_targeted_figures():
    root = Path(__file__).resolve().parent
    paths = sorted((root / "results" / "shild").glob("*.pdf"))
    paths += sorted((root / "results" / "controlled").glob("*.pdf"))
    assert len(paths) == 8
    reports = []
    for path in paths:
        xml = subprocess.check_output(["pdftotext", "-bbox", str(path), "-"], text=True)
        tree = ET.fromstring(xml)
        pages = tree.findall(".//{*}page")
        assert len(pages) == 1, path.name
        page = pages[0]
        width, height = float(page.attrib["width"]), float(page.attrib["height"])
        words = page.findall(".//{*}word")
        assert words, path.name
        for word in words:
            a = word.attrib
            assert 0 <= float(a["xMin"]) <= float(a["xMax"]) <= width, (path.name, word.text)
            assert 0 <= float(a["yMin"]) <= float(a["yMax"]) <= height, (path.name, word.text)
        fonts = subprocess.check_output(["pdffonts", str(path)], text=True).splitlines()[2:]
        assert fonts and all("Arial" in row and "yes yes yes" in row for row in fonts), path.name
        reports.append({
            "file": str(path.relative_to(root)),
            "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
            "width_in": width / 72,
            "height_in": height / 72,
            "pages": 1,
            "text_words": len(words),
            "all_text_inside_page": True,
            "embedded_arial": True,
        })
    report = {"status": "passed", "date": "2026-09-10", "figures": reports}
    output = root / "qa" / "PDF_GEOMETRY_CHECK.json"
    output.write_text(json.dumps(report, indent=2) + "\n")
    print("Passed geometry and embedded-Arial checks for all eight targeted PDFs.")


if __name__ == "__main__":
    verify_targeted_figures()
