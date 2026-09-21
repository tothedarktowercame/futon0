#!/usr/bin/env python3
"""Render shareable SVG artifacts from a measured JSON file (no transcript reads)."""
import argparse
import csv
import json
from pathlib import Path
import sys
sys.path.insert(0,'/home/joe/code/marimo-zone')
from audit_pilot import pilot


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--metrics',type=Path,default=Path(__file__).parent/'metrics-minilm.json')
    parser.add_argument('--out',type=Path,default=Path(__file__).parent)
    parser.add_argument('--threshold',type=float,default=.75)
    parser.add_argument('--repeat-threshold',type=float,default=.8)
    a=parser.parse_args();pairs,labels,_,_=pilot.load();rows=json.loads(a.metrics.read_text())['rows']
    a.out.mkdir(parents=True,exist_ok=True)
    for name,svg in [('stance-distribution',pilot.distribution(pairs,labels)),('stance-over-time',pilot.time_plot(pairs,labels,rows,a.repeat_threshold)),('contribution-by-stance',pilot.contribution_plot(rows,a.threshold))]:
        (a.out/(name+'.svg')).write_text(svg+'\n')
    with (a.out/'measurements.csv').open('w') as f:
        fields=['pair_id','session','operator_at','label','unit_count','contribution_055','contribution_065','contribution_075','contribution_085','repeated_correction_cosine','earlier_correction_id']
        writer=csv.DictWriter(f,fieldnames=fields,lineterminator="\n");writer.writeheader()
        for row in rows:
            result={k:row[k] for k in fields if k in row}
            for t,key in [(.55,'055'),(.65,'065'),(.75,'075'),(.85,'085')]:result['contribution_'+key]=pilot.contribution(row,t)
            writer.writerow(result)

if __name__=='__main__':main()
