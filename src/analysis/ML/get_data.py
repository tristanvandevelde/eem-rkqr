import pandas as pd
import os

def get_data(hour):
    # import data
    data = pd.read_csv(f"/../../../data/final_{hour}.csv")
    data["datetime"] = pd.to_datetime(data["datetime"])

    # make lagged variables
    # BE
    data["priceBE_lag1"] = data["priceBE"].shift(1)
    data["priceBE_lag2"] = data["priceBE"].shift(2)
    data["priceBE_lag3"] = data["priceBE"].shift(3)
    data["priceBE_lag4"] = data["priceBE"].shift(4)
    data["priceBE_lag5"] = data["priceBE"].shift(5)
    # NL
    data["priceNL_lag1"] = data["priceNL"].shift(1)
    data["priceNL_lag2"] = data["priceNL"].shift(2)
    data["priceNL_lag3"] = data["priceNL"].shift(3)
    data["priceNL_lag4"] = data["priceNL"].shift(4)
    data["priceNL_lag5"] = data["priceNL"].shift(5)
    # FR
    data["priceFR_lag1"] = data["priceFR"].shift(1)
    data["priceFR_lag2"] = data["priceFR"].shift(2)
    data["priceFR_lag3"] = data["priceFR"].shift(3)
    data["priceFR_lag4"] = data["priceFR"].shift(4)
    data["priceFR_lag5"] = data["priceFR"].shift(5)
    # DE
    data["priceDE_lag1"] = data["priceDE"].shift(1)
    data["priceDE_lag2"] = data["priceDE"].shift(2)
    data["priceDE_lag3"] = data["priceDE"].shift(3)
    data["priceDE_lag4"] = data["priceDE"].shift(4)
    data["priceDE_lag5"] = data["priceDE"].shift(5)
    # drop other variables
    data.drop(["priceNL", "priceFR", "priceDE"], axis=1, inplace=True)

    # cleanup
    data.drop(["loadFR"], axis=1, inplace=True)
    data.dropna(inplace=True)
    data.drop_duplicates(subset="datetime", keep="last", inplace=True)

    # train/test split
    data = data[data["datetime"].dt.year < 2022]
    data_train = data[data["datetime"].dt.year < 2021]
    data_test = data[(data["datetime"].dt.year == 2021)] 
    X_train = data_train.drop(["datetime", "priceBE"], axis=1)
    y_train = data_train["priceBE"]
    X_test = data_test.drop(["datetime", "priceBE"], axis=1)
    y_test = data_test["priceBE"]

    return X_train, y_train, X_test, y_test