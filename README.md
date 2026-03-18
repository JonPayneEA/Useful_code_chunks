# Useful Code Chunks

This repository contains a collection of useful code snippets and functions across various programming languages. The goal is to provide quick reference access to commonly used programming tasks and algorithms.

## Comprehensive Function List
### FMP_Node_Geometry.R
- **parse_gxy():** Parses .gxy files to LINESTRING sf (spatial features) per reach
- **parse_dat():** Parses .dat files to POINT sf per node, or optionally LINESTRING per reach
- **model_geometry():** Wrapper that tries GXY first, falls back to DAT, and attaches model_id
- **batch_model_geometry():** Applies model_geometry() across a data frame of multiple models

Feel free to contribute and add more snippets or improve the existing ones!
