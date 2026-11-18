class ProcessPV:

    name = "PV Process"

    def PosValue(self, x):
        try:
            if x < 0:
                return 0
            else:
                return x
        except:
            return 0

    def process(self, data, config_process = None):
        data["y"] = data["y"].apply(lambda x: self.PosValue(x))

        return data

import dill
forge = ProcessPV()

with open("forge_positive_pv.dill", "wb") as f:
    dill.dump(forge, f)
