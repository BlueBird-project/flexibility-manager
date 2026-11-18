class BaseForge:

    name = "base_forge"

    def process(self, data, config_process=None):
        raise NotImplementedError


class LoadPriceDB(BaseForge):
    """STUB. Price data will come from a database (no data yet).

    Expected contract when implemented: return a DataFrame with unique_id, ds, y
    using config_process["connection"] / ["query"] (see FAST/forge/pricefm_data.py).
    """

    name = "load_price_db"

    def process(self, data=None, config_process=None):
        raise NotImplementedError(
            "Price loader not implemented yet: return unique_id, ds, y from the price database."
        )


import dill

forge = LoadPriceDB()

with open("forge_load_price_db.dill", "wb") as f:
    dill.dump(forge, f)
