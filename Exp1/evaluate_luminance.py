# This script was used to compute the perceived luminance (accounting for how human eyes perceive brightness differently for different colors)
from PIL import Image
from pathlib import Path
import re
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from skimage import color

def natural_key(s):
    """
    Split a string into chunks of numbers and non-numbers,
    to allow natural sorting (e.g., 1,2,3,10 instead of 1,10,2,3).
    Example: 'image10.png' -> ['image', 10, '.png']
    """
    parts = re.split(r'(\d+)', s)
    return [int(p) if p.isdigit() else p.lower() for p in parts]

in_dir = Path("Images/")
exts = {".jpg", ".jpeg"}
# sort the files in human-friendly order
files = [p for p in in_dir.rglob("*") if p.suffix.lower() in exts]
files.sort(key=lambda p: natural_key(p.name))

# compute the luminance of each image
luminance_list = []
for idx, p in enumerate(files, start=1):

    # open image
    img = Image.open(p)
    rgb = np.array(img.convert('RGB'))

    # Weighted luminance formula (ITU-R BT.601)
    # luminance = 0.2126 * rgb[:, :, 0] + 0.7152 * rgb[:, :, 1] + 0.0722 * rgb[:, :, 2]
    # CIELAB standard
    lab = color.rgb2lab(rgb)
    luminance = lab[:,:,0]

    avg_luminance = luminance.mean()
    luminance_list.append([idx, p, avg_luminance])


luminance_df = pd.DataFrame(data=luminance_list, columns=["ImageNumber", "ImageName", "Luminance"])
luminance_df.to_csv("Image_luminance.csv", index=False)

# evaluate the effect of category on luminance
# load the experimental data
df_img = pd.read_csv("Data_exp1.csv",usecols=["ImageNumber", "Category"]).drop_duplicates()
df_luminance = pd.merge(left=df_img, right=luminance_df, on="ImageNumber")
print(df_luminance.shape)
print(df_luminance.groupby("Category").size())


# Plots
import seaborn as sns
plt.figure(figsize=(8,7))
sns.boxplot(x="Category", y="Luminance", data=df_luminance)
sns.stripplot(x="Category", y="Luminance", data=df_luminance, color="black", size=3, alpha=0.5)
plt.title("Perceived luminance by Category")
plt.xticks(rotation=45)
plt.show()



# ANOVA
import statsmodels.api as sm
from statsmodels.formula.api import ols

# one-way ANOVA
model = ols("Luminance ~ C(Category)", data=df_luminance).fit()
anova_table = sm.stats.anova_lm(model, typ=2)
print("=== One-way ANOVA ===")
print(anova_table)
