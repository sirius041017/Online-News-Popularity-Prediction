"""Run all notebook code cells in order, without starting a Jupyter server."""
from pathlib import Path
import json
import os
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

root = Path(__file__).resolve().parents[1]
os.chdir(root)
notebook = json.loads((root / 'notebooks/01_data_cleaning_and_eda.ipynb').read_text())
namespace = {'__name__': '__main__'}
plt.show = lambda: plt.close('all')
for index, cell in enumerate(notebook['cells']):
    if cell['cell_type'] == 'code':
        exec(compile(''.join(cell['source']), f'notebook-cell-{index}', 'exec'), namespace)
