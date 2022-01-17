#!/Usr/bin/env python3

from pathlib import Path
import shutil
import sys


def eprint(*args, **kwargs):
    print(*args, **kwargs, file=sys.stderr)


if sys.platform == "linux":
    prof_dir = Path.home() / ".zotero/zotero"
elif sys.platform == "darwin":
    prof_dir = Path.home() / "Library/Application Support/Zotero/Profiles"
else:
    eprint(f"Unsupported platform:", sys.platform)
    sys.exit(1)

profiles = list(prof_dir.iterdir())
if not profiles:
    # fmt: off
    eprint(">>> No Zotero profile found. Please install Zotero first and run it at least once.")
    eprint(">>> Zotero: https://www.zotero.org/download/")
    eprint(">>> Better BibTeX: https://github.com/retorquere/zotero-better-bibtex/releases/latest")
    eprint(">>> ZotFile: http://zotfile.com/")
    # fmt: on
    sys.exit(1)
elif len(profiles) > 1:
    eprint(">>> Found multiple profiles, please select which one to use:")
    for i, profile in enumerate(profiles):
        eprint(">>>", i, profile)
    profile = profiles[int(input(">>> Enter profile number: "))]
else:
    profile = profiles[0]

user_js = profile / "user.js"
zotero_dir = Path.home() / ".local" / "share" / "zotero"
storage_dir = zotero_dir / "storage"
zotfile_dir = Path.home() / "Literature"
synced_storage_dir = zotfile_dir / "__zotero-storage__"
eprint(">>> Writing config to", user_js)
user_js.write_text(
    rf"""
// CREATED BY {__file__}
// DO NOT MODIFY!
// MODIFY THAT SCRIPT INSTEAD.
user_pref("extensions.zotero.dataDir", "{zotero_dir}");
user_pref("extensions.zotero.purge.tags", true);
user_pref("extensions.zotero.sync.fulltext.enabled", false);
user_pref("extensions.zotero.sync.server.username", "dlukes");
user_pref("extensions.zotero.sync.storage.enabled", false);
user_pref("extensions.zotero.translators.better-bibtex.caching", true);
user_pref("extensions.zotero.translators.better-bibtex.citeCommand", "cite");
user_pref("extensions.zotero.translators.better-bibtex.citekeyFold", true);
user_pref("extensions.zotero.translators.better-bibtex.citekeyFormat", "[auth:lower][shorttitle3_3][year]");
user_pref("extensions.zotero.translators.better-bibtex.workers", 4);
user_pref("extensions.zotfile.tablet", true);
user_pref("extensions.zotfile.tablet.dest_dir", "{zotfile_dir}");
user_pref("extensions.zotfile.tablet.projectFolders", 2);
user_pref("extensions.zotfile.tablet.subfolders", "[{{\"label\":\"books\",\"path\":\"/books\"}}]");
""".strip(),
    encoding="utf-8",
)

eprint(">>> Creating Zotero data dir at", zotero_dir)
zotero_dir.mkdir(parents=True, exist_ok=True)

if not synced_storage_dir.is_dir():
    eprint(">>>", synced_storage_dir, "should be a dir with Zotero attachments.")
    sys.exit(1)
eprint(">>> Symlinking", synced_storage_dir, "to", storage_dir)
if storage_dir.is_symlink():
    storage_dir.unlink()
storage_dir.symlink_to(synced_storage_dir)

default_zotero_dir = Path.home() / "Zotero"
default_storage_dir = default_zotero_dir / "storage"
if default_zotero_dir.is_dir():
    eprint(">>> Found default Zotero data dir", default_zotero_dir, " -- deleting.")
    delete = True
    if list(default_storage_dir.iterdir()):
        eprint(">>> WARNING:", default_storage_dir, "is not empty.")
        delete = input(">>> Still delete? [Y/n] ").casefold().startswith("y")
    if delete:
        shutil.rmtree(default_zotero_dir)
