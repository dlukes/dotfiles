#!/usr/bin/env python3

import json
from pathlib import Path
import shutil
import sqlite3
import sys


def eprint(*args, **kwargs):
    print(">>>", *args, **kwargs, file=sys.stderr)


# ---------------------------------------------------------------------- Setup detection {{{1


if sys.platform == "linux":
    prof_dir = Path.home() / ".zotero/zotero"
elif sys.platform == "darwin":
    prof_dir = Path.home() / "Library/Application Support/Zotero/Profiles"
else:
    eprint(f"ERROR: Unsupported platform:", sys.platform)
    sys.exit(1)

profiles = prof_dir.is_dir() and list(
    p for p in prof_dir.iterdir() if p.name != "profiles.ini"
)
if not profiles:
    # fmt: off
    eprint("ERROR: No Zotero profile found. Please install Zotero first and run it at least once.")
    eprint("  - Zotero: https://www.zotero.org/download/")
    eprint("  - Better BibTeX: https://github.com/retorquere/zotero-better-bibtex/releases/latest")
    eprint("  - ZotFile: http://zotfile.com/")
    eprint("  - Night for Zotero: https://github.com/tefkah/zotero-night")
    # fmt: on
    sys.exit(1)
elif len(profiles) > 1:
    eprint("Found multiple profiles, please select which one to use:")
    for i, profile in enumerate(profiles):
        eprint(i, profile)
    profile = profiles[int(input(">>> Enter profile number: "))]
else:
    profile = profiles[0]


# --------------------------------------------------------------------- Path definitions {{{1


user_js = profile / "user.js"
lit_dir = Path.home() / "Literature"
zotero_dir = Path.home() / ".local" / "share" / "zotero"
storage_dir = zotero_dir / "storage"
better_bibtex_db = zotero_dir / "better-bibtex.sqlite"
synced_storage_dir = lit_dir / "zotero"
zotfile_dir = lit_dir / "zotfile"
cache_dir = Path.home() / ".cache" / "zotero"
my_library_json = str(cache_dir / "My Library.json")


# ---------------------------------------------------- Creating directories and symlinks {{{1


eprint("Creating Zotero data dir at", zotero_dir)
zotero_dir.mkdir(parents=True, exist_ok=True)

if not synced_storage_dir.is_dir():
    eprint("ERROR:", synced_storage_dir, "should be a dir with Zotero attachments.")
    sys.exit(1)
eprint("Symlinking", synced_storage_dir, "to", storage_dir)
if storage_dir.is_symlink():
    storage_dir.unlink()
storage_dir.symlink_to(synced_storage_dir)

default_zotero_dir = Path.home() / "Zotero"
default_storage_dir = default_zotero_dir / "storage"
if default_zotero_dir.is_dir():
    eprint("Deleting default Zotero data dir:", default_zotero_dir)
    delete = True
    if default_storage_dir.is_dir() and list(default_storage_dir.iterdir()):
        eprint("WARNING:", default_storage_dir, "is not empty.")
        delete = input(">>> Still delete? [Y/n] ").casefold().startswith("y")
    if delete:
        shutil.rmtree(default_zotero_dir)

eprint("Creating Zotero cache dir at", cache_dir)
cache_dir.mkdir(parents=True, exist_ok=True)


# ---------------------------------------------------------------- Preferences (user.js) {{{1


# Generate citekey based on ShortTitle if non-empty, fall back to Title.
citekey = (
    "authEtal2(sep='_').postfix('_').replace(/_etal_$/i, '_et_al_') + "
    "year.postfix('_') + "
    "ShortTitle.len.lower.skipwords.nopunct.condense('_').substring(1,25).replace(/(_[^_]{0,2})+$/, '')"
)
citekey = f"{citekey} | {citekey.replace('ShortTitle.len', 'Title')}"

eprint("Writing config to", user_js)
user_js.write_text(
    rf"""
// CREATED BY {__file__}
// DO NOT MODIFY!
// MODIFY THAT SCRIPT INSTEAD.

// Zotero
user_pref("extensions.zotero.dataDir", "{zotero_dir}");
user_pref("extensions.zotero.purge.tags", true);
user_pref("extensions.zotero.sync.fulltext.enabled", false);
user_pref("extensions.zotero.sync.server.username", "dlukes");
user_pref("extensions.zotero.sync.storage.enabled", false);

// Better BibTeX
user_pref("extensions.zotero.translators.better-bibtex.DOIandURL", "doi");
// Requires Biber 2.7 / BibLaTeX 3.5.
user_pref("extensions.zotero.translators.better-bibtex.biblatexExtendedNameFormat", false);
user_pref("extensions.zotero.translators.better-bibtex.caching", true);
user_pref("extensions.zotero.translators.better-bibtex.citeCommand", "cite");
user_pref("extensions.zotero.translators.better-bibtex.citekeyFold", true);
// For the citekey format, you can use | to specify fallback formats, should the first
// one yield an empty key. There's a default random fallback which is always applied.
// This is your new _-joined citekey format, which takes advantage of tweaked
// soft-wrapping in Emacs 28.1+ to avoid ugly ragged right margins. It uses a substring
// prefix of the title for more predictable length, removing short tokens at the end if
// any.
user_pref("extensions.zotero.translators.better-bibtex.citekeyFormatEditing", "{citekey}");
// Use only a few skipwords to keep title prefixes recognizable.
user_pref("extensions.zotero.translators.better-bibtex.skipWords", "a,an,and,et,le,la,les,the,un,une");
// Citekey search can slow down startup on large libraries, and I don't think I've ever
// used it.
user_pref("extensions.zotero.translators.better-bibtex.citekeySearch", false);
user_pref("extensions.zotero.translators.better-bibtex.keyConflictPolicy", "change");
// Switch to org-cite quick copy if it's ever added, but drag&drop support isn't
// crucial for my use case. In any case, Export → Default Format in the Zotero prefs
// needs to be set to Better BibTeX Quick Copy for this to apply.
user_pref("extensions.zotero.translators.better-bibtex.quickCopyMode", "orgRef");
user_pref("extensions.zotero.translators.better-bibtex.skipFields", "notes");
user_pref("extensions.zotero.translators.better-bibtex.workers", 4);
// Include non-standard file field for attachments in CSL exports, with PDFs as
// zotero://open-pdf links. This whole postscript is quite brittle, see your Org-roam
// for notes on how to debug it if (when) it (inevitably) breaks.
user_pref("extensions.zotero.translators.better-bibtex.postscript", "if (Translator.BetterCSLJSON) {{ csl.file = zotero.attachments.map(a => (/\\.pdf$/i.test(a.localPath) ? `zotero://open-pdf/library/items/${{a.itemKey}}?${{a.localPath.split(a.itemKey)[1].substring(1)}}` : a.localPath).replace(/([\\\\;])/g, \"\\\\$1\")).join(\";\") }}");

// Zotfile
user_pref("extensions.zotfile.authors_delimiter", ",");
user_pref("extensions.zotfile.etal", "_et_al");
user_pref("extensions.zotfile.renameFormat", "{{%a}}-{{%y}}-{{%t}}");
user_pref("extensions.zotfile.renameFormat_patent", "{{%a}}-{{%y}}-{{%t}}");
user_pref("extensions.zotfile.replace_blanks", true);
user_pref("extensions.zotfile.tablet", true);
user_pref("extensions.zotfile.tablet.dest_dir", "{zotfile_dir}");
user_pref("extensions.zotfile.tablet.projectFolders", 2);
user_pref("extensions.zotfile.tablet.subfolders", "[{{\"label\":\"books\",\"path\":\"/books\"}}]");

// Night for Zotero
user_pref("extensions.night.default_pdf", "dark");
user_pref("extensions.night.enabled", true);
""".strip(),
    encoding="utf-8",
)


# ----------------------------------------------------------------- Preferences (SQLite) {{{1


if not better_bibtex_db.is_file():
    eprint("ERROR:", better_bibtex_db, "doesn't exist, can't check automatic exports.")
    sys.exit(1)

# NOTE: Looks like there's more to creating a Zotero export than just adding an entry to
# the Better BibTeX database. Which sort of makes sense -- exports are a Zotero feature
# which Better BibTeX extends, so there's probably other stuff to trigger (in
# particular, that $loki field seems to play an important role). So the best I can do is
# to warn myself to add the export manually.

# eprint("Configuring automatic exports in", better_bibtex_db)
# exports = [
#     {
#         "type": "library",
#         "id": 1,
#         "path": my_library_json,
#         "status": "done",
#         "translatorID": "f4b52ab0-f878-4556-85a0-c7aeedd09dfc",
#         "meta": {"revision": 0, "created": 1645362289004, "version": 0},
#         "$loki": 17,
#     }
# ]

con = sqlite3.connect(better_bibtex_db)
query = "select data from `better-bibtex` where name = 'better-bibtex.autoexport'"
autoexport = json.loads(next(con.execute(query))[0])
existing_exports = {export["path"] for export in autoexport["data"]}
if my_library_json not in existing_exports:
    eprint(
        "WARNING: Missing updated Better CSL-JSON export of entire library at",
        my_library_json,
    )

# for export in exports:
#     if export["path"] not in existing_exports:
#         autoexport["data"].append(export)
# with con:
#     con.execute(
#         "update `better-bibtex` set data = ? where name = 'better-bibtex.autoexport'",
#         (json.dumps(autoexport),),
#     )
