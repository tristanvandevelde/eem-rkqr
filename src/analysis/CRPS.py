import pandas as pd
import numpy as np

def CRPS(predictions, observations):
    results = pd.DataFrame([])
    for column in predictions:
        delta = np.array(predictions[column].values - observations.values)
        results[predictions[column].name] = np.square(float(predictions[column].name) - np.heaviside(delta, 1))
    return results.sum(axis=1).mean()

