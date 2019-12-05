##!/usr/bin/env python

import vcf, re, os, csv, psycopg2, psycopg2.extras, prettytable, itertools

vcf_fn = 'g/invitae/projects/exome/did_data/EX001/analysis_v1/data/EX001_hc.vcf'

# load HGNC idents
genes_fn = 'g/invitae/projects/exome/etc/RP-known.hgnc'
gene_set_known = set(l.strip() for l in open(genes_fn))
gene_set_o7 = set(['CRX','FSCN2','PRPF8','RDH12','RHO','ROM1','RP1L1'])

# get gene bounds
conn = psycopg2.connect(host='localhost',
                        cursor_factory = psycopg2.extras.DictCursor)
cur = conn.cursor()
cur.execute('select gene,chr,strand,start_i,end_i,summary,descr from transcripts1.gene')
gene_info = cur.fetchall()
gene_info.sort( key = lambda gi: gi['chr'] )
gi_by_gene_map = dict([ (gi['gene'],gi) for gi in gene_info ])
gi_by_chr_map = dict([ (g,sorted(gi,key=lambda gi: (gi['chr'],gi['start_i'],gi['end_i'])))
                       for g,gi in itertools.groupby( gene_info, lambda gi: gi['chr'] ) ])


def vcf_table(vrecs):
    pt = prettytable.PrettyTable(field_names=['chr','se','gene','ref','alt','type','qual','EX001-1','EX001-2'])
    for vr in vrecs:
        pt.add_row( [vr.CHROM, (vr.start,vr.end),
                     vr.genes,
                     vr.REF, vr.ALT, vr.var_type, vr.QUAL,
                     str(vr.genotype('EX001-1').data) if vr.genotype('EX001-1').called else None,
                     str(vr.genotype('EX001-2').data) if vr.genotype('EX001-2').called else None ] )
    return pt


def vcf_add_gene(vrecs,gi_by_chr_map):
    """gene assignment by primitive linear search... KBJ will moan"""
    def _overlap(gi,vr):
        return gi['end_i'] >= vr.start and gi['start_i'] <= vr.end
    def _genes_for_vr(vr,gi_by_chr_map):
        cm = gi_by_chr_map[vr.CHROM]
        genes = [ gi['gene'] for gi in cm if _overlap(gi,vr) ] 
        return genes
    for vr in vrecs:
        vr.genes = _genes_for_vr(vr,gi_by_chr_map)


# read vcf
vcfr = vcf.Reader(open(vcf_fn))
vrecs = list(vcfr)
vcf_add_gene(vrecs, gi_by_chr_map)

# qf = qual filter; u1 = unique to EX001-1; u1k = u1 filtered to "known" gene set
vrecs_qf = [ vr for vr in vrecs if vr.QUAL>=40 ]
vrecs_u1 = [ vr for vr in vrecs_qf if vr.genotype('EX001-1').is_variant and not vr.genotype('EX001-2').is_variant ]
vrecs_u1k = [ vr for vr in vrecs_u1 if any(vrg in gene_set_known for vrg in vr.genes) ]
vrecs_u1o7 = [ vr for vr in vrecs_u1 if any(vrg in gene_set_o7 for vrg in vr.genes) ]

