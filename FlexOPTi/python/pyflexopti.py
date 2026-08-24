"""PyFlexOPTi — Python wrapper around the FlexOPTi Julia package.

FlexOPTi is driven as a subprocess: inputs and outputs are JSON files, so
no Julia/Python bridge (PyCall, PyJulia) is needed.

Requirements:
  * a `julia` executable on PATH (see `julia=` to override)
  * Python 3.7+ — no third-party packages, standard library only

Example
-------
>>> from pyflexopti import optimize
>>> result = optimize(
...     dt_file="data/montcada/inputs/dynamics_estimator_results.json",
...     sensors_file="data/montcada/inputs/df_predict.json",
...     forecast_file="data/montcada/inputs/dynamics_estimator_results.json",
...     pilot="Montcada",
...     Hu=2,
... )
>>> result["OPTTerminationStatus"]
'OPTIMAL'
"""

from __future__ import annotations

import json
import pathlib
import subprocess
import tempfile

__all__ = ["optimize", "FlexOPTiError"]

# Repository layout: <repo>/python/flexopti.py -> <repo>
_REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent
_RUN_SCRIPT = _REPO_ROOT / "scripts" / "run_optimize.jl"


class FlexOPTiError(RuntimeError):
    """Raised when the Julia optimization subprocess fails."""


def _resolve_input(name: str, path: str) -> pathlib.Path:
    """Resolve an input path against the caller's cwd and check it exists.

    Relative paths are resolved against the *caller's* working directory,
    not the package root. The most common mistake is running from the
    parent folder while using paths written relative to the FlexOPTi
    folder, so that case gets an explicit hint.
    """
    resolved = pathlib.Path(path).resolve()
    if resolved.is_file():
        return resolved

    hint = ""
    candidate = (_REPO_ROOT / path).resolve()
    if not pathlib.Path(path).is_absolute() and candidate.is_file():
        hint = (
            f"\n\nThat path does exist relative to the FlexOPTi folder:"
            f"\n    {candidate}"
            f"\nYou are running from:"
            f"\n    {pathlib.Path.cwd()}"
            f"\nEither cd into {_REPO_ROOT}, or pass the full path."
        )

    raise FlexOPTiError(f"{name} not found: {resolved}{hint}")


def optimize(
    dt_file: str,
    sensors_file: str,
    forecast_file: str,
    *,
    pilot: str,
    output_file: str | None = None,
    julia: str = "julia",
    capture_output: bool = False,
    **kwargs,
) -> dict:
    """Run one MPC optimization and return the parsed results.

    Parameters
    ----------
    dt_file, sensors_file, forecast_file
        Paths to the digital twin, sensors, and forecast JSON files.
    pilot
        Pilot name, e.g. ``"Montcada"`` or ``"Ewh"`` (case-sensitive).
    output_file
        Where to write the result JSON. If omitted, a temporary file is
        used and only the returned dict is kept.
    julia
        Julia executable to invoke. Override to pin a specific install.
    capture_output
        If True, Julia's logs are captured instead of streamed to the
        terminal, and included in the error message on failure.
    **kwargs
        Forwarded to ``FlexOPTi.optimize`` (``Hu``, ``solver``,
        ``market_country``, ``compute_datetime``, ``loglevel``, ...).

    Returns
    -------
    dict
        The parsed optimization output.
    """
    if not _RUN_SCRIPT.is_file():
        raise FlexOPTiError(f"runner script not found: {_RUN_SCRIPT}")

    with tempfile.TemporaryDirectory() as tmp:
        tmpdir = pathlib.Path(tmp)
        out_path = (
            pathlib.Path(output_file).resolve() if output_file else tmpdir / "output.json"
        )

        # The subprocess runs with cwd set to the package root, so any
        # relative path from the caller is resolved here, against *their*
        # working directory, before being handed to Julia.
        inputs = {
            "dt_file": dt_file,
            "sensors_file": sensors_file,
            "forecast_file": forecast_file,
        }
        resolved = {k: _resolve_input(k, v) for k, v in inputs.items()}

        config = {
            **{k: str(v) for k, v in resolved.items()},
            "pilot": pilot,
            "output_file": str(out_path),
            **kwargs,
        }

        cfg_path = tmpdir / "config.json"
        cfg_path.write_text(json.dumps(config), encoding="utf-8")

        proc = subprocess.run(
            [julia, f"--project={_REPO_ROOT}", str(_RUN_SCRIPT), str(cfg_path)],
            cwd=_REPO_ROOT,
            capture_output=capture_output,
            text=True,
        )

        if proc.returncode != 0:
            detail = f"\n{proc.stderr}" if capture_output and proc.stderr else ""
            raise FlexOPTiError(
                f"FlexOPTi exited with code {proc.returncode}.{detail}"
            )

        if not out_path.is_file():
            raise FlexOPTiError(f"expected output file was not created: {out_path}")

        return json.loads(out_path.read_text(encoding="utf-8"))
