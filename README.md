# BayesBridgeR

R wrapper for the Python 'bayesbridge' package.

### Installation
```bash
python -m pip install --index-url https://test.pypi.org/simple/ bayesbridge
```
The 'bayesbridge' package depends on SciPy and NumPy, so make sure they are installed.

### Usage
Call via the R 'reticulate' package by setting the path to Python installation e.g.
```R
reticulate::use_python("\\Users\\username\\anaconda3\\python.exe"", required = TRUE)
bayesbridge <- reticulate::import('bayesbridge')
```
