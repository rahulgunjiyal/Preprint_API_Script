#API Request Script for Data Acquisition 

import requests
import time
import csv
from pathlib import Path
from datetime import date
from concurrent.futures import ThreadPoolExecutor

START_DATE = "2013-01-01"
END_DATE   = "2024-12-01"

BATCH_SIZE = 100
MAX_WORKERS = 10
SLEEP_BETWEEN_BATCHES = 1.0
TIMEOUT = 20

BASE_URL = "https://api.biorxiv.org/details/biorxiv"

OUTPUT_DIR = Path("biorxiv_Data")
XML_DIR = OUTPUT_DIR / "jats_xml"
CSV_FILE = OUTPUT_DIR / "ppc_dataset.csv"


CSV_FIELDS = [
    "ppc_id",
    "source",
    "doi",
    "title",
    "authors",
    "date",
    "category",
    "abstract",
    "author_corresponding",
    "author_corresponding_institution",
    "version",
    "submission",
    "server",
    "published",
    "published_doi",
    "publication_citation",
    "total_citation",
    "jatsxml",
    "xml_local"
]


def ensure_dirs():
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    XML_DIR.mkdir(parents=True, exist_ok=True)

def sanitize_filename(s):
    return s.replace("/", "_").replace(":", "_")

def fetch_published_doi(preprint_doi):
    if not preprint_doi:
        return None
    try:
        r = requests.get(
            f"https://api.biorxiv.org/pubs/biorxiv/{preprint_doi}",
            timeout=TIMEOUT
        )
        if r.status_code == 200:
            coll = r.json().get("collection", [])
            if coll:
                return coll[0].get("published_doi")
    except requests.RequestException:
        pass
    return None

def fetch_citation_count(doi):
    if not doi:
        return 0
    try:
        r = requests.get(
            f"https://api.crossref.org/works/{doi}?mailto={CROSSREF_MAILTO}",
            timeout=TIMEOUT
        )
        if r.status_code == 200:
            return r.json()["message"].get("is-referenced-by-count", 0)
    except requests.RequestException:
        pass
    return 0

def download_xml(jatsxml, doi):
    if not jatsxml or not doi:
        return None
    out_file = XML_DIR / f"{sanitize_filename(doi)}.xml"
    if out_file.exists():
        return str(out_file)
    try:
        r = requests.get(jatsxml, timeout=TIMEOUT)
        if r.status_code == 200 and r.content.strip():
            out_file.write_bytes(r.content)
            return str(out_file)
    except requests.RequestException:
        pass
    return None



def process_paper(global_idx, art):
    doi = art.get("doi")
    version = art.get("version", "1")

    ppc_id = f"PPC{global_idx:07d}"
    source = "bioRxiv"

    jatsxml = f"https://www.biorxiv.org/content/{doi}v{version}.full.xml"

    xml_local = download_xml(jatsxml, doi)

    published_doi = fetch_published_doi(doi)
    time.sleep(0.05)

    total_citation = fetch_citation_count(doi)
    time.sleep(0.1)

    return [
        ppc_id,
        source,
        doi,
        art.get("title"),
        art.get("authors"),
        art.get("date"),
        art.get("category"),
        art.get("abstract"),
        art.get("author_corresponding"),
        art.get("author_corresponding_institution"),
        version,
        art.get("submission"),
        art.get("server"),
        art.get("published"),
        published_doi,
        None, 
        total_citation,
        jatsxml,
        xml_local
    ]


def main():
    ensure_dirs()

    if not CSV_FILE.exists():
        with CSV_FILE.open("w", newline="", encoding="utf-8") as f:
            csv.writer(f).writerow(CSV_FIELDS)

    cursor = 0
    global_index = 1

    while True:
        print(f"Fetching records starting at cursor {cursor}...")
        url = f"{BASE_URL}/{START_DATE}/{END_DATE}/{cursor}"

        try:
            resp = requests.get(url, timeout=TIMEOUT)
            records = resp.json().get("collection", [])
        except Exception as e:
            print("Fetch error:", e)
            break

        if not records:
            print("All records fetched ")
            break

        with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
            futures = [
                executor.submit(process_paper, global_index + i, art)
                for i, art in enumerate(records)
            ]
            rows = [f.result() for f in futures]

        with CSV_FILE.open("a", newline="", encoding="utf-8") as f:
            csv.writer(f).writerows(rows)

        print(f"Saved {len(rows)} records")

        global_index += len(records)
        cursor += BATCH_SIZE
        time.sleep(SLEEP_BETWEEN_BATCHES)

if __name__ == "__main__":
    main()
