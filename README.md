<div align="center">

<img src="https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/data/haplofun_logo_2.png" alt="Haplofun logo" height="140"/>

**Haplofun: Interactive haplotype analysis with integrated functional annotation**

[![License: MIT](https://img.shields.io/badge/license-MIT-007bff.svg)](https://choosealicense.com/licenses/mit/)
![Version](https://img.shields.io/badge/version-v1.0.0-007bff)
[![GitHub](https://img.shields.io/badge/github-repo-2c3e50?logo=github)](https://github.com/TimHasenbein/Haplofun)
![R](https://img.shields.io/badge/R-4.5.1-2c3e50?logo=r)
![Shiny](https://img.shields.io/badge/built%20with-Shiny-2c3e50?logo=rstudio)

`VCF` · `haplotypes` · `GWAS` · `QTL` · `functional annotation` · `network analysis`

A robust tool for the generation, visualization, and annotation of genetic haplotypes from VCF data.

</div>

---

## Online

Haplofun is available as web server application at **[datascience-fzb.shinyapps.io/haplofun](https://datascience-fzb.shinyapps.io/haplofun/)**.

> [!NOTE]
> The public server enforces a 500 kb VCF file size limit (< 1,000 variants and < 1,000 samples recommended).
> Furthermore, the initial page load may take a while, as the server spins up and loads all required packages and dependencies.
---

## Content

- [1. Data input & compliance](#1-data-input--compliance)
  - [1.1 Data security and compliance](#11-data-security-and-compliance)
  - [1.2 Data upload](#12-data-upload)
  - [1.3 Accessing pre-stored data sets](#13-accessing-pre-stored-data-sets)
  - [1.4 Pre-analysis data inspection](#14-pre-analysis-data-inspection)
- [2. Haplotype network & visualization](#2-haplotype-network--visualization)
  - [2.1 Data filtering](#21-data-filtering)
  - [2.2 Haplogroup frequency table](#22-haplogroup-frequency-table)
  - [2.3 Samples per haplogroup](#23-samples-per-haplogroup)
  - [2.4 Advanced filtering options](#24-advanced-filtering-options)
  - [2.5 Haplotype visualizations](#25-haplotype-visualizations)
- [3. Functional annotation](#3-functional-annotation)
  - [3.1 GWAS annotation of haplotypes](#31-gwas-annotation-of-haplotypes)
  - [3.2 QTL annotation of haplotypes](#32-qtl-annotation-of-haplotypes)
  - [3.3 Genomic annotation of haplogroups](#33-genomic-annotation-of-haplogroups)
- [Citation](#citation)
- [Contact & technical support](#contact--technical-support)
- [License](#license)
- [Installation](#installation)

---

## 1. Data input & compliance

This section covers the data protection declaration, data upload, and the use of pre-stored example data.

### 1.1 Data security and compliance

**Data protection declaration.** The Haplofun web server operates on a non-persistence principle. Uploaded VCF files are not permanently stored on the server; processing occurs exclusively in working memory (RAM) during your active session.

> [!IMPORTANT]
> Regardless of data type, users must declare the nature of their data, and retain full responsibility for adhering to all relevant data protection laws concerning the uploaded material.

| Data type | Requirement |
|---|---|
| Public / non-human data | No login required |
| Private / sensitive human data | Registration and login required |

> [!NOTE]
>The sessions expire after 15 minutes of inactivity.

### 1.2 Data upload

Upon confirming the data protection declaration, the Haplofun workflow begins. The server accepts **phased diploid** or **monoploid** genomic variation data conforming to the **Variant Call Format (VCF)**. Metadata files can additionally be supplied for optional annotation.

**Required files**

| File | Description |
|---|---|
| VCF file (`.vcf` / `.vcf.gz`) | Main data file. Must include sample genotype information (`GT` field). File size limit is **500 kb** on the public server, with < 1,000 variants and < 1,000 samples recommended. Larger files require pre-filtering with `bcftools`. Multiallelic variants must be represented as comma-separated values within a single line rather than split across multiple bi-allelic records. |

**Optional files**

| File | Description |
|---|---|
| Sample information (`.tsv`) | Two-column, tab-delimited file of sample attributes (e.g. population, phenotype). Column 1 holds sample IDs, column 2 holds the metadata for that sample. Used to color the frequency bar and network plots. |
| GWAS information (`.csv`) | Genome-Wide Association Study file for mapping trait-associated SNPs to haplogroups. Comma-separated file with the format: `chromosome, start, end, name, score, strand`, `trait, risk allele, pval, effect_size`. |
| QTL information (`.csv`) | Quantitative Trait Loci file for mapping QTLs to haplogroups. Comma-separated file with the format: `chromosome, start, end, name (REF/effect allele), score, strand`, `tissue, pip, effect size (allelic fold change), target gene`. |
| Annotation information (`.csv`) | Annotation file for mapping genomic features to haplogroups: `chromosome, start, end, name, score, strand`. |

### 1.3 Accessing pre-stored data sets

Besides your own data, you can use bundled public datasets for functional annotation.

> [!TIP]
> Want to try Haplofun without preparing your own files? Load the bundled **1000G (*IL23R* locus, hg38)** VCF together with the **GWAS catalog**, **eQTL**, and **cCRE** annotation to test the full workflow.

**Pre-stored VCF files**

- **1000G data (*IL23R* locus, hg38)**: 1000 Genomes project data, filtered for variants of the *IL23R* gene locus (`chr1:66,952,344-67,374,700`).
- ***H. influenzae* (*ftsI* locus)**: Example VCF data for the bacterial *ftsI* gene locus (`1,688,312-1,690,102`).

**Pre-stored annotation files**

- **GWAS catalog (hg38)**: Pre-processed human GWAS data from the GWAS catalog, used for risk allele mapping.
- **eQTL data (hg38)**: Pre-processed human expression QTL data (GTEx v10 SuSiE eQTL, hg38, PIP > 0.5).
- **cCREs annotation (ENCODE)**: Candidate cis-regulatory elements from the UCSC Genome Browser (ENCODE).
- ***H. influenzae* (*ftsI* locus)**: Microbial GWAS data for antimicrobial resistance (Diricks, M., Petersen, S., Bartels, L. et al., *Genome Med* 2024).

### 1.4 Pre-analysis data inspection

An initial overview of the uploaded data, before running the haplotype analysis:

- **Data summary**: Genomic range with data entries, and the number of initial samples, variants, haplotypes, and distinct haplogroups.
- **Integrative Genomics Viewer (IGV)**: In-browser VCF visualization. The reference annotation is selected in the *Select Genome Build for IGV* tab.
- **VCF header table**: Position, variant ID, alleles, quality, and filters.
- **VCF data lines**: The variant records themselves.

## 2. Haplotype network & visualization

Core analysis of haplotypes, combining quantitative data with comprehensive visualization. The interface is organized into three interactive components: data filtering and SNP sub-sampling, the haplotype frequency table, and the haplotype visualizations.

### 2.1 Data filtering

A continuously updated overview reflecting the current filtered data state:

- Total number of **samples**
- Total number of **variants (SNPs)**
- Total number of **haplotypes**
- Total number of **haplogroups**

**Filtering options** (the data summary updates dynamically after any filter is applied):

- **Genomic location**: Restrict the analysis to a specific chromosomal region defined by start and end coordinates.
- **Minor allele frequency (MAF)**: Exclude rare variants by setting a minimum MAF threshold.

### 2.2 Haplogroup frequency table

- **Content**: Lists the frequency of haplogroups and the allele observed at each locus across the defined genomic region.
- **Reactivity**: Content depends on the "Number of top haplogroups" selection and the active filters, prioritizing the most frequent haplogroups (max. 50).

### 2.3 Samples per haplogroup

- **Content**: Lists the IDs of samples present in each haplogroup.
- **Reactivity**: Content depends on the genetic variants selected.

### 2.4 Advanced filtering options

- **Linkage disequilibrium (LD) pruning**: Subsets the data based on LD, calculated across the variants in your dataset. Multiallelic variants cannot be processed reliably due to limitations of the pegas package. For variant pairs above the r² threshold, the first variant of the pair is retained.
- **Random SNP sub-sampling**: Randomly selects a specified number of variants from the current filtered dataset, used to assess the robustness and variability of the inferred haplogrouptype network. Comparing networks from multiple random SNP subsets shows how stable the genetic relationships are for the predefined variant set.

### 2.5 Haplotype visualizations

Six interactive plots derived from the VCF input, for structural analysis of the haplogroups. All plots are dynamically linked to the "Number of top haplogroups" control in the sidebar. The plots can be downloaded as PDF files.

| Plot | Description |
|---|---|
| **Frequencies** (bar plot) | Frequencies of samples per haplogroup. |
| **Distance matrix** (heatmap) | Hamming distance matrix for the selected haplogroups, with optional clustering (`ward.D`, `single`, `complete`, `average`, `mcquitty`, `median`, `centroid`). |
| **Network** | Network plot for the selected haplogroups as computed and visualized via the pegas R package. Four model methods: Haplonet, Minimum Spanning Network (MSN), Randomized Minimum Spanning Tree (RMST), and Minimum Spanning Tree (MST), plus fast-plotting options for larger datasets. Includes allele/haplogroup coloring, frequency-scaled nodes, adjustable plot size/scale/labels/color, an edge-count threshold, and edge weight display as lines, dots, or numbers. |
| **PCA** | Principal component analysis (PC1 vs. PC2) for the selected haplogroups; dot size scaled to haplogroup sample frequency. |
| **Dendrogram** | Hierarchical clustering of the selected haplogroup, with the same agglomeration methods as the distance matrix. |
| **Allele heatmap** | Visual alignment of the alleles per haplogroup, depicted by color. |

## 3. Functional annotation

Functional annotation of haplogroups was designed for GWAS alleles, QTLs, and genomic elements, but any genomic element can be annotated as long as the input file follows the structure described in [1.2 Data upload](#12-data-upload).

**Metadata annotation.** Enriches the visual analysis by colorizing the haplotype frequency bar plot and genetic network plot according to sample traits, such as geographic location or phenotype. The metadata file must follow the format described in [1.2 Data upload](#12-data-upload).

<div align="center">
<img src="https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/data/meta_2.png" alt="Metadata coloring of the frequency bar plot and haplotype network" width="720"/>

*Figure 1: The frequency bar plot and haplotype network, colored by sample metadata.*
</div>

### 3.1 GWAS annotation of haplotypes

Insights into risk or trait allele mapping across inferred haplotypes, for identifying trait-relevant haplogroups, across four interactive panels:

- **GWAS variants in haplogroups**: All GWAS-associated variants within the selected haplogroups: genomic position, associated trait, risk allele, p-value, and effect size.
- **Haplotype allele table (GWAS)**: Alleles found at the GWAS/trait-associated positions for each inferred haplogroup: a visual alignment confirming allelic composition at trait-related sites.
- **GWAS alleles mapping**: Heatmap of risk allele presence across haplogroups; cell color denotes the associated trait.
- **GWAS effect sizes**: Genomic position vs. effect size; node size reflects statistical significance, color denotes the SNP-trait association.

All tables and plots can be dynamically filtered for a specific SNP-trait pair. The network can be colored by the selected associations, with node size proportional to the number of present associations.

### 3.2 QTL annotation of haplotypes

Insights into QTL allele mapping across inferred haplogroups, for identifying regulatory-associated haplogroups, across four interactive panels:

- **QTL variants in haplogroups**: All QTL-associated variants within the selected haplogroups: genomic position, tissue and alleles, posterior inclusion probability (PIP), and effect sizes.
- **Haplogroup allele table (QTLs)**: Alleles found at the QTL positions for each haplogroup.
- **QTL allele mapping**: Heatmap of effect allele presence across haplogroups; cell color denotes the associated target gene per allele.
- **QTL effect sizes**: Genomic position vs. effect size; lollipop node color corresponds to the target gene per allele, node size reflects the PIP.

All tables and plots can be dynamically filtered for QTL-target pairs. The network can be colored by the selected pairs, with node size proportional to the number of present QTLs.

### 3.3 Genomic annotation of haplogroups

Feature mapping of haplogroups, to identify whether a variant overlaps a genomic feature, across four interactive panels:

- **Variants overlapping annotation**: All data variants of the haplogroups that overlap a genomic feature: genomic position, name (ID), type of regulatory element, and its position.
- **Haplotype allele table (annotation)**: Alleles of the variant overlapping a genomic feature.
- **Annotation mapping**: Heatmap of the allele of a variant overlapping a genomic feature; The color denotes the bases (A: green; C: blue; G: yellow; T: red).
- **Count of VCF variants overlapping annotation features**: Quantification of variants overlapping different genomic features; color represents the distinct features.

All tables and plots can be dynamically filtered for a specific feature of interest.

---

## Citation

If you use Haplofun in your research, please cite:

> Hasenbein TP., Bartels L., Stolze R., Wohlers I. (2026). Haplofun: Interactive haplotype analysis with integrated functional annotation. *JOURNAL*. DOI: [to be inserted upon publication]

> [!NOTE]
> Until the publication is available, please cite the web server URL and acknowledge the research group.

## Contact & technical support

For technical support, bugs, or inquiries regarding data submission and collaboration, please contact the development team:

| | |
|---|---|
| **Contact** | Prof. Dr. Inken Wohlers |
| **Affiliation** | Biomolecular Data Science in Pneumology, Research Center Borstel, Germany |
| **Email** | [shiny-datascience@fz-borstel.de](mailto:shiny-datascience@fz-borstel.de) |
| **Lab homepage** | [fz-borstel.de](https://fz-borstel.de/de/forschung-am-fzb/wissenschaft-und-technologie/data-science-in-der-lungenforschung) |

## License

This project is licensed under the [MIT License](https://choosealicense.com/licenses/mit/).

&copy; Biomolecular Data Science in Pneumology, Research Center Borstel, Leibniz Lung Center, Germany.

---

## Installation

Prefer running the app locally? Haplofun can be downloaded and run entirely on your own machine, which allows custom changes eg. for the file-size limits imposed on the public server.

**Prerequisites:** R **4.5.1** (the version this app is locked against) and [renv](https://rstudio.github.io/renv/).

```r
# R
install.packages("renv")
```
Clone the github repository:
```bash
# bash 
git clone https://github.com/TimHasenbein/Haplofun.git
```

Restore the exact package environment recorded in `renv.lock`:

```r
# R
setwd("./Haplofun/00_app")
renv::restore()
```

Then launch the app:

```r
shiny::runApp()
```
