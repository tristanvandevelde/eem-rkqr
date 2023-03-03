import pandas as pd
import numpy as np

def CRPS(predictions, observations):
    results = pd.DataFrame([])
    i = 0.01
    for column in predictions:
        delta = np.array(predictions[column].values - observations.values)
        results[predictions[column].name] = np.square(i - np.heaviside(delta, 1))
        i += 0.01
    return results.sum(axis=1).mean()

