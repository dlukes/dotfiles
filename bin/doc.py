#!/usr/bin/env python3

import os
import logging as log
from shlex import quote
from pathlib import Path

import click as cli
import regex as re

NAME = Path(__file__).stem
LOG = log.getLogger(NAME)
LOGLEVELS = [
    s
    for f, s in sorted(
        (v, k) for k, v in vars(log).items() if k.isupper() and isinstance(v, int)
    )
]

BIB_DIR = Path(Path.home(), ".files", "texmf", "bibtex", "bib", "local")
BUILD_SCRIPT = r"""
#!/usr/bin/env zsh

cd ${{0:a:h}}
pandoc {args} \
  --number-sections \
  -F pandoc-crossref \
  -V papersize=a4 \
  --bibliography pandoc.bib
""".strip()


def parse_bib(bib_str: str, entries: dict):
    for m in re.finditer(
        r"(@.*?\{(.*?),.*?^\}$)", bib_str, flags=re.DOTALL | re.MULTILINE
    ):
        entry, id_ = m.groups()
        id_ = "@" + id_
        if id_ in entries:
            LOG.warning(f"Duplicate ID {id_} in bib database!")
        entries[id_] = entry


def load_bibs(bib_dir: Path):
    entries = {}
    for bib_path in bib_dir.glob("**/*.bib"):
        with open(bib_path) as file:
            parse_bib(file.read(), entries)
    return entries


def extract_references(input_file_paths, ref_re):
    referenced = set()
    for path in input_file_paths:
        txt = Path(path).read_text()
        referenced.update(m.group() for m in ref_re.finditer(txt))
    return referenced


def write_bib_file(entries, referenced):
    referenced = [entries[id_] for id_ in sorted(referenced)]
    with open("pandoc.bib", "w") as file:
        print("\n\n".join(referenced), file=file)


def get_customizers(paths):
    """Customizers are appropriately named templates or reference docs."""
    customizers = []
    for path in paths:
        path = Path(path)
        suffix = path.suffix
        template = path.with_name(suffix[1:]).with_suffix(".tmpl")
        if template.is_file():
            customizers.append(f"--template {quote(str(template))}")
            log.info(f"Found template: {template}")
        ref_doc = path.with_name("ref").with_suffix(suffix)
        if ref_doc.is_file():
            customizers.append(f"--reference-doc {quote(str(ref_doc))}")
            log.info(f"Found reference document: {ref_doc}")
    return customizers


def write_build_script(input, output, pandoc_args):
    customizers = get_customizers(output)
    input_args = [f"-i {quote(i)}" for i in input]
    output_args = [f"-o {quote(o)}" for o in output]
    args = input_args + output_args + customizers
    args.extend(pandoc_args)
    args = " ".join(args)
    script_path = Path("pandoc.sh")
    with script_path.open("w") as file:
        print(BUILD_SCRIPT.format(args=args), file=file)
    script_path.chmod(0o755)
    return script_path


@cli.command()
@cli.option(
    "lvl",
    "--log",
    help="Set logging level.",
    type=cli.Choice(LOGLEVELS),
    default="WARN",
)
@cli.option("--verbose", "-v", help="(Repeatedly) increase logging level.", count=True)
@cli.option("--quiet", "-q", help="(Repeatedly) decrease logging level.", count=True)
@cli.option(
    "--input", "-i", type=cli.Path(dir_okay=False), multiple=True, required=True
)
@cli.option(
    "--output", "-o", type=cli.Path(dir_okay=False), multiple=True, required=True
)
@cli.argument("pandoc_args", nargs=-1)
def main(lvl, verbose, quiet, input, output, pandoc_args):
    """Generate a pandoc build script and run it.

    Multiple input and output paths can be specified. Positional arguments are
    passed on to pandoc (specify options after `--` to pass them to pandoc).

    For an output format with extension `.ext`, you can specify a template or a reference doc by
    putting a file named `ext.tmpl` or `ref.ext` in the same directory as the document(s) being
    compiled. The default template for a given format is dumped with `pandoc -D FORMAT`. Available
    output formats are listed with `pandoc --list-output-formats`. `.docx` has no template, use
    reference docs to customize output.

    """
    lvl = getattr(log, lvl) - 10 * verbose + 10 * quiet
    log.basicConfig(
        level=lvl, format="[%(asctime)s {}:%(levelname)s] %(message)s".format(NAME)
    )
    bib_entries = load_bibs(BIB_DIR)
    # using this re, extract all matching IDs from input files, then select bib entries based on
    # that
    id_union = "|".join(re.escape(id_) for id_ in bib_entries.keys())  # type: ignore
    bib_id_re = re.compile(r"(?<![-_\+\d\w])(" + id_union + r")(?![-_\+\d\w])")
    log.info(f"Scanning {input!r} for bibliographical references")
    referenced = extract_references(input, bib_id_re)
    log.info(f"Found the following bibliographical references: {referenced!r}")
    write_bib_file(bib_entries, referenced)
    script_path = write_build_script(input, output, pandoc_args)
    os.execv(script_path.absolute(), [script_path])


if __name__ == "__main__":
    main()
