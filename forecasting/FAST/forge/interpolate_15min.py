class Interpolate15Min:

    name = "interpolate_15min"

    import pandas as pd


    def interpolate_timeseries_15min(
        self,
        df,
        id_col="Cups",
        time_col="ds",
        freq="15min"
    ):
        """
        Interpola series temporales separadas por `id_col` a una frecuencia fija
        usando interpolación cúbica.

        Parameters
        ----------
        df : pd.DataFrame
            Dataset original
        id_col : str
            Columna identificadora de series (ej. 'Cups')
        time_col : str
            Columna de timestamps en formato string (ej. 'ds')
        freq : str
            Frecuencia objetivo (por defecto '15min')

        Returns
        -------
        pd.DataFrame
            Dataset interpolado con frecuencia regular
        """

        import pandas as pd

        df = df.copy()

        # Convertir ds a datetime
        df[time_col] = pd.to_datetime(df[time_col])

        interpolated_series = []

        for cup_id, g in df.groupby(id_col):
            g = g.sort_values(time_col).set_index(time_col)

            # Reindexar a una grilla temporal regular
            full_index = pd.date_range(
                start=g.index.min(),
                end=g.index.max(),
                freq=freq
            )
            g = g.reindex(full_index)

            # Aplicar interpolación cúbica solo a columnas numéricas
            num_cols = g.select_dtypes(include="number").columns
            
            if len(g[num_cols].dropna()) >= 4:
                try:
                    g[num_cols] = g[num_cols].interpolate(
                        method="spline",
                        order=3,
                        limit_direction="both"
                    )
                except:
                    g[num_cols] = g[num_cols].interpolate(
                    method="linear",
                    limit_direction="both")
            else:
                # Fallback seguro
                g[num_cols] = g[num_cols].interpolate(
                    method="linear",
                    limit_direction="both")
                


            # Restaurar identificador
            g[id_col] = cup_id
            g = g.reset_index().rename(columns={"index": time_col})

            interpolated_series.append(g)

        return pd.concat(interpolated_series, ignore_index=True)

    def process(self, data: pd.DataFrame, config_process: dict):
        
        data_processed =  self.interpolate_timeseries_15min(
            df=data,
            id_col=config_process.get("id_col", "unique_id"),
            time_col=config_process.get("time_col", "ds"),
            freq=config_process.get("freq", "15min")
        )

        #####(data_processed.head(3))

        return data_processed

import dill

forge = Interpolate15Min()

with open("forge_interpolate_15min.dill", "wb") as f:
    dill.dump(forge, f)