# Multiple color panel -----------------
c68 <- c("#4A6DCB","#22B4FB","#E67CC3","#D0A5F8","#EEB3EA","#4F7392","#AFB2B7","#B39180","#666666","#A56DAD","#EAA86C","#FD5312","#8900C1","#C46DA0","#E3CFE7","#538BE9","#8CB890","#FFF59B","#3955A1","#E18772","#D14C4C","#EACF68","#CAA57D","#EBDBE4","#ECAFCF","#F6C985","#C35338","#EAA944","#63B472","#DEEAB1","#F7DDD4","#F3746C","#F8BFAF","#BB4A94","#B9D0D8","#DC4AA8","#7673AE","#9E6BAB","#D6D4EB","#EF5276","#D7A9CB","#9F82A4","#277AB6","#A79388","#74517B","#69B4CE","#D5E7F7","#66CEF6","#CBEBF6","#F4ACBD","#37A2F1","#F8F4A8","#228B22","#F39800","#009B9F","#d95f02","#E066FF","#80b1d3","#1C86EE","#D1BABA","#B03060","#00008B","#FFE4B5","#C71585","#529C47","#E64B35","#b3de69","#6495ED")

# Personalized color panel----
Binomial_color_panel <- c("TRUE" = "#E64B35", "FALSE" = "lightgrey")
Global_Cluster_color_panel <- c("B cell" = "#69B4CE", "CD4 T cell" = "#ECAFCF", "CD8 T cell" = "#BB4A94",  "ILC" = "#A0D7C9", "Myeloid cell" = "#EAA944", "Epithelial cell" = "#CAA57D", "Fibroblast" = "#F3746C", "Malignant cell" = "#C35338", "Unknown cell" = "#C35338", "Lymphocyte" = "#F3746C")
Sub_Cluster_color_panel <- c(
  "hB01_PlasmaB-IgG" = "#4A6DCB","hB02_GALTB-IgA" = "#22B4FB","hB03_FollicularB-IgD" = "#E67CC3","hB04_FollicularB-MS4A1" = "#D0A5F8","hB05_GCBCell-LRMP" = "#EEB3EA",
  "hI01_NK-CD16" = "#FFF59B","hI02_NK-GZMK" = "#3955A1","hI03_NK-CD103" = "#E18772","hI04_ILC3-SLC4A10" = "#D14C4C",
  "hM01_Mast-TPSAB1" = "#EACF68","hM02_pDC-LILRA4" = "#CAA57D","hM03_cDC2-CD1C" = "#EBDBE4","hM04_cDC1-BATF3" = "#ECAFCF","hM05_Mono-CD14" = "#F6C985",
  "hM06_Mono-CD16" = "#C35338","hM07_Mono-CD14CD16" = "#EAA944","hM08_Macro-NLRP3" = "#63B472","hM09_Macro-PLTP" = "#DEEAB1", "hM10_Macro-IL1B" = "#F7DDD4",
  "hM11_Monolike-FCN1" = "#F3746C","hM12_TAM-C1QC" = "#F8BFAF","hM13_TAM-SPP1" = "#BB4A94",
  "hT01_CD4-CCR7" = "#B9D0D8","hT02_CD4-ANXA1" = "#DC4AA8","hT03_CD4-GNLY" = "#7673AE","hT04_CD4-TCF7" = "#9E6BAB","hT05_CD4-CXCR6" = "#D6D4EB",
  "hT06_CD4-CXCR5" = "#EF5276","hT07_CD4-GZMK" = "#D7A9CB","hT08_CD4-IL23R" = "#9F82A4","hT09_CD4-CXCL13" = "#277AB6","hT10_CD4-FOXP3" = "#A79388",
  "hT11_CD4-CTLA4" = "#74517B","hT12_CD8-LEF1" = "#69B4CE","hT13_CD8-GPR183" = "#D5E7F7","hT14_CD8-CX3CR1" = "#66CEF6","hT15_CD8-GZMK" = "#CBEBF6",
  "hT16_CD8-CD6" = "#F4ACBD","hT17_CD8-CD160" = "#37A2F1","hT18_CD8-LAYN" = "#F8F4A8",
  "hE01_Endothelium-ACKR1" = "#A56DAD","hE02_Enterocyte-GUCA2B" = "#EAA86C","hE03_Goblet-SPINK4" = "#FD5312","hE04_Goblet-REG4" = "#8900C1","hE05_Stemlike-LGR5" = "#C46DA0", "hE06_UnIdent" = "#E3CFE7",
  "hF01_Myofib-ACTA2" = "#538BE9","hF02_CAF-FAP" = "#8CB890",
  "hC01_P0413" = "#4F7392","hC02_P0825" = "#AFB2B7","hC03_P0411" = "#B39180","hC04_P1212" = "#666666",
  "rI01_NK-Klrd1" = "#FFF59B",
  "rT01_CD4-Gpr183" = "#F39800","rT02_CD4-Ctla4" = "#F8BFAF","rT03_CD8-Mki67" = "#d95f02","rT04_CD8-Pdcd1" = "#E066FF",
  "mB01_BCell-Cd79b" = "#80b1d3",
  "mI01_NK-Klrb1c" = "#228B22", 
  "mL01_NKT-Cd163l1" = "#D1BABA", "mL02_IEL-Cd7" = "#B03060", "mL03_Doublet" = "#00008B",
  "mM01_Mast-Cpa3" = "#EACF68", "mM02_Neutrophil-Csf3r" = "#1C86EE", "mM03_pDC-Siglech" = "#CAA57D", "mM04_cDC2-Cd209a" = "#22B4FB", "mM05_cDC2-Itgax" = "#009B9F",
  "mM06_cDC1-Clec9a" = "#ECAFCF", "mM07_cDC1-Ccl22" = "#F3746C", "mM08_Mono-Ly6c2" = "#63B472", "mM09_Mono-Nr4a1" = "#4A6DCB", "mM10_Mono-Itgal" = "#9E6BAB",
  "mM11_Macro-Mafb" = "#F6C985", "mM12_Macro-Maf" = "#69B4CE", "mM13_Macro-Ccl12" = "#228B22", "mM14_Macro-Mgl2" = "#F8BFAF", "mM15_Macro-Vegfa" = "#BB4A94",
  "mT01_CD4_Tn-Lef1" = "#FFE4B5", "mT02_CD4_Tcm-Ifit3" = "#F39800", "mT03_CD4_Th-Bhlhe40" = "#C71585", "mT04_CD4_Treg-Foxp3" = "#529C47", "mT05_CD4_Tfh-Cxcr5" = "#EF5276",
  "mT06_CD4_Tcm-Tcf7" = "#7673AE", "mT07_CD8_Tn-Ccr7" = "#009B9F", "mT08_CD8_Tcm-Ifit3" = "#E64B35", "mT09_CD8_Tem-Ccl5" = "#b3de69", "mT10_CD8_Trm-Cxcr6" = "#6495ED", 
  "mT11_CD8_Tex-Lag3" = "#E066FF", "mT12_CD8_Tex-Mki67" = "#d95f02")
Tissue_color_panel <- c("T" = '#E64B35', "Tumor" = '#E64B35', "N" = '#00A087', "Normal" = '#00A087', "P" = '#F0CE39', "Blood" = '#F0CE39', "LN" = "#8da0cb") 
Sample_color_panel <- c(
  "P0104" = "#CBB7DF", "P0305" = "#EBDBE4", "P0309" = "#63B472", "P0411" = "#F6C985", "P0413" = "#F7DDD4", 
  "P0720" = "#DEEAB1", "P0728" = "#F8F4A8", "P0825" = "#D5E7F7", "P1212" = "#BB4A94", "P1228" = "#ECAFCF", 
  "P0123" = "#D69782", "P0202" = "#9E6BAB", "P0323" = "#66CEF6", "P0408" = "#7AAF93", "P0410" = "#F3746C", 
  "P0613" = "#D6D4EB", "P1025" = "#EACF68", "P1026" = "#69B4CE", "P0313" = "#7A8DBB", "P0330" = "#A0D7C9",
  "XCM01" = "#CBB7DF", "XCM02" = "#EBDBE4", "XCM03" = "#63B472", "XCM04" = "#F6C985", "XCM05" = "#F7DDD4", 
  "XCM06" = "#DEEAB1", "XCM07" = "#F8F4A8", "XCM08" = "#D5E7F7", "XCM09" = "#BB4A94", "XCM10" = "#ECAFCF", 
  "XCM11" = "#D69782", "XCM12" = "#9E6BAB", "XCM13" = "#66CEF6", "XCM14" = "#7AAF93")
Treatment_color_panel <- c(
  "Isotype" = "#1b9e77", "CD40" = "#d95f02", "CSF1R" = "#d95f02"
)
Day_color_panel <- c(
  "Ten" = "maroon", "Two" = "skyblue2"
)
