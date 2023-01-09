#!/usr/bin/env python3
"""Show diffs for base vs. remote and base vs. local for each merge conflict in a file.

"""
from dataclasses import dataclass
from pathlib import Path
import subprocess as sp
import sys
from tempfile import TemporaryDirectory
import shutil


@dataclass
class MergeConflict:
    lines: list[str]
    local: int | None = None  # The commit you're rebasing *on*.
    base: int | None = None  # The reconstructed merge base.
    remote: int | None = None  # The commit you're rebasing.
    end: int | None = None

    def render(self) -> bytes:
        assert self.local is not None
        assert self.base is not None
        assert self.remote is not None
        local = "".join(self.lines[self.local + 1 : self.base])
        base = "".join(self.lines[self.base + 1 : self.remote])
        remote = "".join(self.lines[self.remote + 1 : self.end])
        with TemporaryDirectory() as dirname:
            dir = Path(dirname)
            localp, basep, remotep = dir / "local", dir / "base", dir / "remote"
            localp.write_text(local, encoding="utf-8")
            basep.write_text(base, encoding="utf-8")
            remotep.write_text(remote, encoding="utf-8")
            diff_local = self.diff(basep, localp)
            diff_remote = self.diff(basep, remotep)
        return diff_local + b"\n\n" + diff_remote + b"\n\n"

    @staticmethod
    def diff(a: Path, b: Path) -> bytes:
        cp = sp.run(
            ["git", "diff", "--no-ext-diff", "--color=always", "--", str(a), str(b)],
            capture_output=True,
        )
        assert cp.stderr == b""
        ans = cp.stdout
        for prefix, path in ((b"a", a), (b"b", b)):
            ans = ans.replace(prefix + str(path).encode(), path.name.encode())
        return ans


with open(sys.argv[1]) as file:
    lines = file.readlines()

mcs: list[MergeConflict] = []
mc = MergeConflict(lines)
for i, line in enumerate(lines):
    match line[:7]:
        case "<<<<<<<":
            mc.local = i
        case "|||||||":
            mc.base = i
        case "=======":
            mc.remote = i
        case ">>>>>>>":
            mc.end = i
            mcs.append(mc)
            mc = MergeConflict(lines)

last = len(mcs) - 1
for i, mc in enumerate(mcs):
    sys.stdout.buffer.write(mc.render())
    if i < last:
        input("Press any key to continue.")
        print("-" * shutil.get_terminal_size().columns)
