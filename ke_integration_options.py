"""Generate the two FM <-> Trading Manager Knowledge Engine integration options as PNG.

Option A: Python KE sidecar container (fm-ke) that bridges KE <-> SQLite.
Option B: Julia talks to the KE runtime REST API directly (no extra FM container).
"""
from matplotlib.patches import FancyBboxPatch
import matplotlib.pyplot as plt

# ── colour palette (same as docker_architecture.py) ──────────────────────────
C_BG      = "#0d1117"
C_BB      = "#1e3a5f"   # FM network bg
C_TM      = "#1a3a1a"   # TM network bg
C_SVC     = "#1c2b3a"
C_TM_SVC  = "#1a2e1a"
C_EDGE    = "#2a4a6a"
C_TM_EDGE = "#2a5a2a"
C_NEW     = "#3a2a10"   # new component
C_NEW_E   = "#fa4"
C_VOL     = "#2a2a1a"
C_TXT     = "#eeeeee"
C_DIM     = "#9a9a9a"
C_ARR     = "#4af"
C_ARR2    = "#4f4"
C_KE      = "#fa4"
C_FALL    = "#888888"


def new_fig(title, subtitle):
    fig, ax = plt.subplots(figsize=(16, 11))
    ax.set_xlim(0, 16)
    ax.set_ylim(0, 11)
    ax.axis("off")
    fig.patch.set_facecolor(C_BG)
    ax.set_facecolor(C_BG)
    ax.text(8, 10.6, title, ha="center", va="center", fontsize=14,
            fontweight="bold", color=C_TXT, fontfamily="monospace")
    ax.text(8, 10.2, subtitle, ha="center", va="center", fontsize=9,
            color=C_DIM, fontfamily="monospace")
    return fig, ax


def box(ax, x, y, w, h, label, lines=(), color=C_SVC, edge=C_EDGE, port=None,
        label_top=False, lw=1.5):
    ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0.05",
                                facecolor=color, edgecolor=edge, linewidth=lw))
    if label_top:
        ty = y + h - 0.25
    else:
        ty = y + h / 2 + 0.13 * len(lines)
    ax.text(x + w / 2, ty, label, ha="center", va="center", fontsize=9.5,
            fontweight="bold", color=C_TXT, fontfamily="monospace")
    for i, line in enumerate(lines):
        ax.text(x + w / 2, ty - 0.3 - 0.24 * i, line, ha="center", va="center",
                fontsize=7.5, color=C_DIM, fontfamily="monospace")
    if port:
        ax.text(x + w - 0.08, y + 0.08, port, ha="right", va="bottom",
                fontsize=7, color="#fa4", fontfamily="monospace")


def net_rect(ax, x, y, w, h, label, color, edge):
    ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0.1",
                                facecolor=color, edgecolor=edge, linewidth=2,
                                linestyle="--", alpha=0.55))
    ax.text(x + 0.15, y + h - 0.12, label, ha="left", va="top", fontsize=8.5,
            color=edge, fontweight="bold", fontfamily="monospace")


def arrow(ax, x0, y0, x1, y1, color=C_ARR, both=False, dashed=False, lw=1.6):
    ax.annotate("", xy=(x1, y1), xytext=(x0, y0),
                arrowprops=dict(arrowstyle="<|-|>" if both else "-|>",
                                color=color, lw=lw, mutation_scale=12,
                                linestyle="--" if dashed else "-"))


def label(ax, x, y, text, color=C_ARR, ha="center", size=7.5):
    ax.text(x, y, text, ha=ha, va="center", fontsize=size, color=color,
            fontfamily="monospace", linespacing=1.35)


def tm_side(ax):
    """Trading Manager stack (identical in both options)."""
    net_rect(ax, 9.2, 2.2, 6.5, 7.6, "tm-net  (Trading Manager / PSNC)", C_TM, "#4f4")
    box(ax, 9.7, 7.6, 5.6, 1.3, "tm-ke-service  — Knowledge Engine runtime",
        ["TNO smart-connector 1.5.0  ·  /rest/"],
        color="#2a2410", edge=C_KE, port=":8280", lw=2)
    box(ax, 9.7, 5.0, 2.7, 1.2, "tm-service", ["Python/FastAPI", "REST /api + KE KB"],
        color=C_TM_SVC, edge=C_TM_EDGE, port=":9090")
    box(ax, 12.7, 5.0, 2.6, 1.2, "entsoe-service", ["day-ahead prices", "(KE KB)"],
        color=C_TM_SVC, edge=C_TM_EDGE)
    box(ax, 9.7, 2.7, 2.7, 1.0, "tm-db", ["Postgres · markets/offers"],
        color=C_VOL, edge="#888")

    arrow(ax, 11.05, 6.25, 11.05, 7.55, color=C_KE, both=True)
    label(ax, 11.15, 6.9, "ANSWER\ntm-info,\ntou-price*", color=C_KE, ha="left", size=7)
    arrow(ax, 14.0, 6.25, 14.0, 7.55, color=C_KE)
    label(ax, 14.1, 6.9, "POST\nmarket\noffers", color=C_KE, ha="left", size=7)
    arrow(ax, 11.05, 4.95, 11.05, 3.75, color=C_ARR2, both=True)
    label(ax, 11.15, 4.35, "read/write offers", color=C_ARR2, ha="left", size=7)


def notes(ax, pros, cons, footer):
    ax.text(0.4, 1.95, "+ pros", fontsize=9, fontweight="bold", color="#4f4",
            fontfamily="monospace", va="top")
    for i, p in enumerate(pros):
        ax.text(0.4, 1.6 - 0.27 * i, "· " + p, fontsize=8, color=C_TXT,
                fontfamily="monospace", va="top")
    ax.text(8.2, 1.95, "− cons", fontsize=9, fontweight="bold", color="#f66",
            fontfamily="monospace", va="top")
    for i, c in enumerate(cons):
        ax.text(8.2, 1.6 - 0.27 * i, "· " + c, fontsize=8, color=C_TXT,
                fontfamily="monospace", va="top")
    ax.text(8, 0.12, footer, ha="center", va="bottom", fontsize=7.5,
            color=C_DIM, fontfamily="monospace", style="italic")


FOOTER = ("KE runtime: share the TM's tm-ke-service (must be reachable from the FM network), "
          "or run an own smart-connector linked via a Knowledge Directory.")

# ═════════════════════════════════════════════════════════════════════════════
#  Option A — Python KE sidecar
# ═════════════════════════════════════════════════════════════════════════════
fig, ax = new_fig("Option A — Python KE sidecar (fm-ke) + SQLite handoff",
                  "fc stays KE-agnostic: prices arrive in the DB like forecasts do")
net_rect(ax, 0.3, 2.2, 8.2, 7.6, "flexibility-manager_default  (FM)", C_BB, "#4af")
tm_side(ax)

box(ax, 0.8, 7.6, 3.2, 1.3, "fc", ["Julia · FlexOPTi MPC", "price_source = \"db\""], port=":8080")
box(ax, 0.8, 5.0, 3.2, 1.4, "db-data  (SQLite)", ["/data/bb.db", "forecasts · setpoints", "+ prices  (new table)"],
    color=C_VOL, edge="#888")
box(ax, 0.8, 2.7, 3.2, 1.0, "dummy-forecast", ["synthetic disturbances"], port=":5000")
box(ax, 5.0, 2.7, 3.0, 1.0, "control-room", ["Flask dashboard"], port=":9000")
box(ax, 5.0, 5.0, 3.0, 1.4, "fm-ke   (NEW)", ["Python container", "ke_client + UBFlex", "polls every delta_t"],
    color=C_NEW, edge=C_NEW_E, lw=2.2)

arrow(ax, 2.4, 7.55, 2.4, 6.45, both=True)
label(ax, 2.5, 7.0, "reads prices+forecasts\nwrites setpoints", ha="left", size=7)
arrow(ax, 2.4, 3.75, 2.4, 4.95)
label(ax, 2.5, 4.35, "forecasts", ha="left", size=7)
arrow(ax, 4.95, 5.7, 4.05, 5.7, both=True)
label(ax, 4.5, 6.05, "prices →\n← setpoints", size=6.5)
arrow(ax, 5.6, 3.75, 3.6, 4.95)
label(ax, 4.35, 4.15, "queries", size=7)

arrow(ax, 7.6, 6.45, 9.65, 8.0, color=C_KE, both=True, lw=2)
label(ax, 6.6, 8.8, "KE REST:\nASK tm-info, tou-price*\nANSWER fm-ts* (later)",
      color=C_KE, size=7)

arrow(ax, 4.05, 8.0, 9.65, 5.8, color=C_FALL, dashed=True)
label(ax, 4.75, 7.0, "fallback:\nHTTP /api\n(price_source=\"http\")",
      color=C_FALL, ha="left", size=7)

notes(ax,
      pros=["fits current design: DB is the integration bus",
            "reuses PSNC's ke_client + UBFlex models (maintained upstream)",
            "Julia MPC unchanged except reading prices from DB",
            "same container can later answer TM's fm-ts / evaluate requests"],
      cons=["one extra container (Python image) to build and run",
            "depends on tm.* model files + private PSNC pip index",
            "price freshness depends on fm-ke poll cadence",
            "two runtimes (Julia + Python) in the FM stack"],
      footer=FOOTER)
fig.savefig("ke_option_A_sidecar.png", dpi=180, facecolor=C_BG, bbox_inches="tight")
plt.close(fig)

# ═════════════════════════════════════════════════════════════════════════════
#  Option B — Julia native KE client
# ═════════════════════════════════════════════════════════════════════════════
fig, ax = new_fig("Option B — Julia talks to the Knowledge Engine directly",
                  "no extra FM container: the KE client lives inside fc")
net_rect(ax, 0.3, 2.2, 8.2, 7.6, "flexibility-manager_default  (FM)", C_BB, "#4af")
tm_side(ax)

box(ax, 0.8, 5.9, 5.6, 3.2, "fc   (Julia · FlexOPTi)", [], port=":8080", label_top=True)
box(ax, 1.05, 6.35, 2.0, 2.05, "MPC core", ["mpc_update", "build_dynamics", "fetch prices"])
box(ax, 3.55, 6.35, 2.65, 2.05, "ke_client.jl (NEW)",
    ["register KB + KIs", "UBFlex graph patterns", "RDF ⇄ Julia bindings", "long-poll /sc/handle", "(async @task)"],
    color=C_NEW, edge=C_NEW_E, lw=2.2)
arrow(ax, 3.12, 7.35, 3.48, 7.35, both=True, color=C_TXT, lw=1.2)

box(ax, 0.8, 3.9, 3.2, 1.2, "db-data  (SQLite)", ["/data/bb.db", "forecasts · setpoints"],
    color=C_VOL, edge="#888")
box(ax, 0.8, 2.45, 3.2, 0.8, "dummy-forecast", [], port=":5000")
box(ax, 5.0, 2.45, 3.0, 0.8, "control-room", [], port=":9000")

arrow(ax, 2.1, 5.85, 2.1, 5.15, both=True)
label(ax, 2.2, 5.5, "forecasts ↓  setpoints ↑", ha="left", size=7)
arrow(ax, 2.4, 3.3, 2.4, 3.85)
arrow(ax, 5.6, 3.3, 4.05, 4.3)
label(ax, 5.0, 4.05, "queries", size=7, ha="left")

arrow(ax, 6.25, 8.0, 9.65, 8.25, color=C_KE, both=True, lw=2)
label(ax, 7.45, 8.85, "KE REST:\nASK tm-info, tou-price*\nANSWER fm-ts* (later)",
      color=C_KE, size=7)

arrow(ax, 6.45, 6.4, 9.65, 5.7, color=C_FALL, dashed=True)
label(ax, 7.0, 5.75, "fallback: HTTP /api", color=C_FALL, ha="left", size=7)

notes(ax,
      pros=["no extra container; one language in the FM stack",
            "prices fetched exactly when the MPC needs them",
            "no dependency on PSNC's Python packages"],
      cons=["re-implement ke_client in Julia (registration, KIs, bindings)",
            "must handle RDF / SAREF / UBFlex encoding by hand",
            "long-polling for ANSWER needs a background task in fc",
            "nobody else maintains it — breaks when UBFlex patterns change",
            "KE issues can stall or crash the MPC loop"],
      footer=FOOTER)
fig.savefig("ke_option_B_julia_native.png", dpi=180, facecolor=C_BG, bbox_inches="tight")
plt.close(fig)

print("Saved ke_option_A_sidecar.png and ke_option_B_julia_native.png")
