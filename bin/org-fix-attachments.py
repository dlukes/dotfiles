#!/usr/bin/env python3

from pathlib import Path
import sys

import pydantic
import regex as re
import rich.console
import rich.progress


class Settings(pydantic.BaseSettings):
    ORG_DIR: Path = Path("~/Desktop/org").expanduser()
    ATTACH_DIR: Path = ORG_DIR / "attach"

    @pydantic.validator("ORG_DIR", "ATTACH_DIR")
    def dir_must_exist(cls, d: Path) -> Path:
        if not d.is_dir():
            err = FileNotFoundError(d)
            err.add_note("HINT: Maybe Emacs/Org mode config changed?")
            raise err
        return d

    class Config:
        env_prefix = "ORG_FIX_ATTACHMENTS_"
        case_sensitive = True


S = Settings()


def refile_attachment(org_id: str, attach_name: str):
    """Move attachment so that it's accessible from `org_id`."""
    (attach_path,) = list(S.ATTACH_DIR.rglob(f"*/{attach_name}"))
    new_attach_path = S.ATTACH_DIR / org_id[:2] / org_id[2:] / attach_name
    new_attach_path.parent.mkdir(parents=True, exist_ok=True)
    attach_path.rename(new_attach_path)


def fix_attachments_in_org_file(org_path: Path) -> int:
    fixes = 0
    org_txt = org_path.read_text(encoding="utf-8")
    org_id = None
    for m in re.finditer(
        r"^\s*:ID:\s+(?<org_id>.*)|\[attachment:(?<attachment>.*?)\]",
        org_txt,
        flags=re.MULTILINE,
    ):
        if m.group("org_id") is not None:
            org_id = m.group("org_id")
        else:
            assert org_id is not None
            attach_path = S.ATTACH_DIR / org_id[:2] / org_id[2:] / m.group("attachment")
            if not attach_path.is_file():
                fixes += 1
                refile_attachment(org_id, m.group("attachment"))
                if not attach_path.is_file():
                    err = FileNotFoundError(attach_path)
                    err.add_note("HINT: Attachment still not found after refiling.")
                    raise err
    return fixes


def main():
    if len(sys.argv) > 1:
        paths = [Path(p) for p in sys.argv[1:]]
    else:
        paths = list(S.ORG_DIR.rglob("*.org"))
    console = rich.console.Console()
    for path in rich.progress.track(paths, console=console):
        if fixes := fix_attachments_in_org_file(path):
            console.log(f"Made {fixes} attachment fixes in {path}.")


if __name__ == "__main__":
    main()
