#!/usr/bin/env python3

offsets = dict(fg=30, bg=40, fg_bright=90, bg_bright=100)
for i in range(8):
    print(
        "  ".join(
            f"[{offset + i}m{desc} = {offset + i}[m"
            for desc, offset in offsets.items()
        )
    )
print("Additional properties (separate with colons):")
for i, prop in {
    1: "bold",
    2: "faint",
    3: "italic",
    4: "underline",
    5: "slow blink",
    6: "fast blink",
}.items():
    print(f"  - {prop}: {i}")
print(r"General format: \e[1;33mCONTENT HERE\e[m")
print(r"If \e not supported, \033 might work, or verbatim escape char with Ctrl-V Esc.")
