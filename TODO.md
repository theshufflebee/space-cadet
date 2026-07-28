---
title: "TODO"
output: html_document
---

- Clean up the data handling -> everything to YYYYQX
- INterest rates on bonds changed source: https://data.snb.ch/en/topics/ziredev/cube/rendeiduebm
 -> find way to handle this
- remove using RDS files for chaching data
- remove read.table and replace with read_scv fpr SNBV files
- Improve the T_0 = ?? determination -> issue is that first calculate dependent stuff like output gap at max available for that variable then cut off at the best available or at a given >= date
- Add yearly inflation estimation, maybe that performs better