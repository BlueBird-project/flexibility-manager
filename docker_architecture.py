"""Generate Docker architecture diagram as PDF."""
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

fig, ax = plt.subplots(figsize=(16, 10))
ax.set_xlim(0, 16)
ax.set_ylim(0, 10)
ax.axis("off")
fig.patch.set_facecolor("#0d1117")
ax.set_facecolor("#0d1117")

# ── colour palette ────────────────────────────────────────────────────────────
C_BB   = "#1e3a5f"   # BlueBird network bg
C_TM   = "#1a3a1a"   # TM network bg
C_SVC  = "#1c2b3a"   # service box
C_TM_SVC = "#1a2e1a" # TM service box
C_EDGE = "#2a4a6a"   # border
C_TM_EDGE = "#2a5a2a"
C_VOL  = "#2a2a1a"   # volume
C_NET  = "#4af"      # network label
C_TXT  = "#eeeeee"
C_DIM  = "#888888"
C_ARR  = "#4af"
C_ARR2 = "#4f4"

def box(ax, x, y, w, h, label, sublabel="", color=C_SVC, edge=C_EDGE, port=None):
    rect = FancyBboxPatch((x, y), w, h,
                          boxstyle="round,pad=0.05",
                          facecolor=color, edgecolor=edge, linewidth=1.5)
    ax.add_patch(rect)
    ax.text(x + w/2, y + h/2 + (0.15 if sublabel else 0),
            label, ha="center", va="center",
            fontsize=9, fontweight="bold", color=C_TXT, fontfamily="monospace")
    if sublabel:
        ax.text(x + w/2, y + h/2 - 0.22,
                sublabel, ha="center", va="center",
                fontsize=7, color=C_DIM, fontfamily="monospace")
    if port:
        ax.text(x + w - 0.08, y + 0.12, port,
                ha="right", va="bottom",
                fontsize=6.5, color="#fa4", fontfamily="monospace")

def net_rect(ax, x, y, w, h, label, color, edge):
    rect = FancyBboxPatch((x, y), w, h,
                          boxstyle="round,pad=0.1",
                          facecolor=color, edgecolor=edge,
                          linewidth=2, linestyle="--", alpha=0.6)
    ax.add_patch(rect)
    ax.text(x + 0.15, y + h - 0.22, label,
            ha="left", va="top",
            fontsize=8, color=edge, fontweight="bold", fontfamily="monospace")

def arrow(ax, x0, y0, x1, y1, color=C_ARR, label="", bidirectional=False):
    style = "<->" if bidirectional else "->"
    ax.annotate("", xy=(x1, y1), xytext=(x0, y0),
                arrowprops=dict(arrowstyle=style, color=color, lw=1.5))
    if label:
        mx, my = (x0+x1)/2, (y0+y1)/2
        ax.text(mx + 0.05, my + 0.12, label,
                ha="center", va="bottom",
                fontsize=6.5, color=color, fontfamily="monospace")

def vol_box(ax, x, y, label):
    rect = FancyBboxPatch((x, y), 1.8, 0.55,
                          boxstyle="round,pad=0.05",
                          facecolor=C_VOL, edgecolor="#888", linewidth=1, linestyle=":")
    ax.add_patch(rect)
    ax.text(x + 0.9, y + 0.275, label,
            ha="center", va="center",
            fontsize=7, color="#ccc", fontfamily="monospace")

# ── Title ─────────────────────────────────────────────────────────────────────
ax.text(8, 9.6, "BlueBird Flexibility Manager — Docker Architecture",
        ha="center", va="center",
        fontsize=13, fontweight="bold", color=C_TXT, fontfamily="monospace")

# ── BlueBird network rect ─────────────────────────────────────────────────────
net_rect(ax, 0.3, 0.5, 9.8, 8.6, "flexibility-manager_default", C_BB, "#4af")

# ── TM network rect ───────────────────────────────────────────────────────────
net_rect(ax, 10.3, 0.5, 5.2, 8.6, "local_tm-net  (external)", C_TM, "#4f4")

# ── FC also on tm-net (overlap indicator) ─────────────────────────────────────
net_rect(ax, 4.1, 3.6, 6.2, 5.2, "fc also joined here", "#0d1a0d", "#4f4")

# ── Services: BlueBird side ───────────────────────────────────────────────────
box(ax, 0.7, 7.2, 2.5, 1.0, "dummy-forecast",  "Python / stdlib",  port=":5000")
box(ax, 0.7, 5.2, 2.5, 1.0, "control-room",    "Flask / Python",   port=":9000")
box(ax, 0.7, 1.2, 2.5, 1.0, "dt-ewh",          "R / ctsmTMB\n(profile: retrain)")

# FC — centre, spans both networks
box(ax, 4.3, 4.8, 3.5, 1.4, "fc  (FlexOPTi)",  "Julia / HiGHS\nMPC :8080",
    color="#1a2a3a", edge="#4af", port=":8080")

# ── Services: TM side ────────────────────────────────────────────────────────
box(ax, 10.7, 6.8, 4.0, 1.0, "tm-service",    "Trading Manager",
    color=C_TM_SVC, edge=C_TM_EDGE, port=":8080")

# ── Volumes ───────────────────────────────────────────────────────────────────
vol_box(ax, 0.7, 3.2, "💾 db-data\n/data/bb.db")
vol_box(ax, 3.3, 2.0, "📂 dt-volume\ncold_room_model.json")

# ── Arrows: data flows ────────────────────────────────────────────────────────
# dummy-forecast → db-data
arrow(ax, 1.95, 7.2, 1.95, 3.75, label="writes\nforecasts")
# fc → db-data (reads + writes)
arrow(ax, 4.3, 5.5, 2.5, 3.55, bidirectional=True, color="#4af", label="read forecasts\nwrite setpoints")
# control-room → db-data
arrow(ax, 1.95, 5.2, 1.95, 3.75, color="#4af", label="reads")
# dt-ewh → dt-volume
arrow(ax, 1.95, 2.2, 3.3, 2.3, color=C_ARR2, label="writes model")
# dt-volume → fc
arrow(ax, 5.2, 2.55, 5.2, 4.8, color=C_ARR2, label="reads model")
# fc → tm-service
arrow(ax, 7.8, 5.5, 10.7, 7.2, color=C_ARR2, label="GET /api/market\nGET /api/price")
# control-room → fc
arrow(ax, 3.2, 5.7, 4.3, 5.7, color="#fa4", label="GET /status\n/health")

# ── Host port exposure ────────────────────────────────────────────────────────
ax.text(0.5, 0.3, "HOST ports:  5000 → dummy-forecast   8080 → fc   9000 → control-room",
        ha="left", va="center",
        fontsize=7.5, color="#fa4", fontfamily="monospace")

# ── Legend ────────────────────────────────────────────────────────────────────
legend_items = [
    mpatches.Patch(facecolor=C_BB,     edgecolor="#4af", linestyle="--", label="flexibility-manager network"),
    mpatches.Patch(facecolor=C_TM,     edgecolor="#4f4", linestyle="--", label="local_tm-net (external)"),
    mpatches.Patch(facecolor=C_VOL,    edgecolor="#888", linestyle=":",  label="Docker volume"),
]
ax.legend(handles=legend_items, loc="lower right",
          facecolor="#181818", edgecolor="#444",
          labelcolor=C_TXT, fontsize=8, framealpha=0.9)

plt.tight_layout()
plt.savefig("docker_architecture.pdf", dpi=150, bbox_inches="tight",
            facecolor=fig.get_facecolor())
plt.savefig("docker_architecture.png", dpi=150, bbox_inches="tight",
            facecolor=fig.get_facecolor())
print("Saved: docker_architecture.pdf + docker_architecture.png")
