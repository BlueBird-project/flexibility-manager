"""Shared request-validation error, kept dependency-free on purpose.

Both engines raise this and `inference_server` turns it into a 400. It lives here rather than in
`state_builder.py` so the MPC path never has to import that module (and therefore numpy): the
MPC image ships with only pulp + highspy.
"""


class StateBuilderError(ValueError):
    """Raised for any malformed or out-of-contract request payload."""
