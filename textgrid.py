#!/usr/bin/env python3

import sys
from praatio import textgrid

def textgrid_to_custom_format(textgrid_file):
    tg = textgrid.openTextgrid(textgrid_file, includeEmptyIntervals=False)
    output = ""

    for tier_name, tier in tg._tierDict.items():
        output += f"{tier.tierType}: {tier_name}\n"

        if isinstance(tier, textgrid.IntervalTier):
            for entry in tier.entries:
                xmin, xmax, label = entry
                output += f"{xmin}-{xmax}:{label}\n"
        else:
            for entry in tier.entries:
                time, label = entry
                output += f"{time}:{label}\n"

    return output

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python textgrid.py <textgrid_file>")
        sys.exit(1)

    textgrid_file = sys.argv[1]
    custom_output = textgrid_to_custom_format(textgrid_file)
    print(custom_output)
