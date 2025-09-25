from pathlib import Path
import re
import numpy as np
import pandas as pd
from PIL import Image, ImageFile, ImageOps
import matplotlib.pyplot as plt

# Allow loading truncated images without raising errors
ImageFile.LOAD_TRUNCATED_IMAGES = True


def natural_key(s):
    """
    Split a string into chunks of numbers and non-numbers,
    to allow natural sorting (e.g., 1,2,3,10 instead of 1,10,2,3).
    Example: 'image10.png' -> ['image', 10, '.png']
    """
    parts = re.split(r'(\d+)', s)
    return [int(p) if p.isdigit() else p.lower() for p in parts]


def hsv_mask(rgb_uint8, h_min=64, h_max=112, s_min=50, v_min=40):
    """
    Generate a boolean mask selecting pixels considered "green" based on HSV thresholds.

    Parameters:
        rgb_uint8: np.ndarray (H, W, 3), input RGB image in uint8.
        h_min, h_max: int, hue thresholds (0–255 in Pillow HSV scale).
        s_min: int, minimum saturation.
        v_min: int, minimum brightness (value).

    Returns:
        Boolean mask (H, W) where True indicates green pixels.
    """
    hsv = Image.fromarray(rgb_uint8, "RGB").convert("HSV")
    hsv_np = np.asarray(hsv)
    H = hsv_np[:, :, 0]
    S = hsv_np[:, :, 1]
    V = hsv_np[:, :, 2]
    return (H >= h_min) & (H <= h_max) & (S >= s_min) & (V >= v_min)


def compute_greenness(images_dir="Images",
                      out_dir="Masks",
                      h_min=64, h_max=112, s_min=50, v_min=40,
                      natural_sort_files=True,
                      zero_pad_output=True):
    """
    Process all images in a folder, calculate greenness ratio, and save results.

    Steps:
      1. Read all images from `images_dir`.
      2. Convert them to HSV and apply thresholds to compute a binary mask.
      3. Compute greenness ratio = (#green pixels) / (#valid pixels).
      4. Save visualization (original + mask) into `out_dir`.
      5. Save results (ImageNumber, ImageName, Greenness) into CSV in current folder.

    Parameters:
        images_dir: str | Path, input folder containing images.
        out_dir: str | Path, output folder for mask visualizations.
        h_min, h_max, s_min, v_min: HSV thresholds.
        natural_sort_files: bool, whether to sort files in human-friendly order.
        zero_pad_output: bool, whether to pad output filenames with zeros.

    Returns:
        pandas.DataFrame containing image info and greenness values.
    """
    in_dir = Path(images_dir)
    out_dir = Path(out_dir);
    out_dir.mkdir(parents=True, exist_ok=True)
    exts = {".jpg", ".jpeg", ".png", ".bmp", ".tif", ".tiff", ".webp"}
    files = [p for p in in_dir.rglob("*") if p.suffix.lower() in exts]

    if not files:
        print(f"[WARN] No images found under {in_dir.resolve()}")
        return None

    # Sort filenames naturally (e.g., 1,2,3,10,11) instead of lexicographic (1,10,11,2,3)
    if natural_sort_files:
        files.sort(key=lambda p: natural_key(p.name))
    else:
        files = sorted(files)

    # Number of digits for zero-padding output filenames
    pad_width = len(str(len(files))) if zero_pad_output else 0

    records = []
    for idx, p in enumerate(files, start=1):
        try:
            im = Image.open(p)

            # Correct orientation using EXIF metadata if available
            try:
                im = ImageOps.exif_transpose(im)
            except Exception:
                pass

            # Handle alpha channel (transparency) if present
            if im.mode == "RGBA":
                rgba = np.asarray(im.convert("RGBA"))
                valid = rgba[..., 3] > 0  # only count non-transparent pixels
                rgb = rgba[..., :3].astype(np.uint8)
            else:
                rgb = np.asarray(im.convert("RGB"), dtype=np.uint8)
                valid = np.ones(rgb.shape[:2], dtype=bool)

            # Apply HSV threshold to compute greenness mask
            m = hsv_mask(rgb, h_min, h_max, s_min, v_min) & valid
            ratio = float(m[valid].mean()) if valid.any() else float("nan")

            # Visualization: create green mask overlay
            vis = np.zeros((*m.shape, 3), dtype=np.uint8)
            vis[m] = [0, 255, 0]

            # Plot original image and mask side by side
            fig, axes = plt.subplots(1, 2, figsize=(10, 6))
            axes[0].imshow(np.asarray(im.convert("RGB")))
            axes[0].set_title(p.name)
            axes[0].axis("off")

            axes[1].imshow(vis)
            axes[1].set_title(f"HSV mask  Greenness={ratio:.4f}\nH[{h_min},{h_max}] S≥{s_min} V≥{v_min}")
            axes[1].axis("off")
            plt.tight_layout()

            # Generate output filename
            if zero_pad_output:
                out_fname = f"{idx:0{pad_width}d}_{p.stem}_mask.png"
            else:
                out_fname = f"{p.stem}_mask.png"
            out_path = out_dir / out_fname

            # Save mask visualization figure
            fig.savefig(out_path, dpi=150)
            plt.close(fig)

            print(f"[OK] {p.name} -> {out_fname} | Greenness={ratio:.4f}")
            records.append({
                "ImageNumber": idx,
                "ImageName": p.name,
                "Greenness": ratio
            })

        except Exception as e:
            print(f"[WARN] Skip {p.name}: {e}")

    # Save results as CSV in current directory
    df = pd.DataFrame(records)
    csv_path = Path("Image_info.csv")
    df.to_csv(csv_path, index=False)
    print(f"[DONE] Saved {len(df)} entries to {csv_path.name}")
    return df


# Example usage with custom HSV thresholds
compute_greenness(images_dir="Images", out_dir="Masks", h_min=60, h_max=112, s_min=40, v_min=40)