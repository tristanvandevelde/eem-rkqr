import requests
import config

## TODO:
## use type and BZ configuration from config.py file

def get_response(year, BZ, type):
    
    """
    This function takes in a year and a bidding zone.
    It calls the API, and returns the API response.
    This can be fed to the data format function.
    """
    
    params = {
        "securityToken": config.security_token,
        "in_Domain": BZ,
        "out_Domain": BZ,
        "periodStart": str(year) + "01010000",
        "periodEnd": str(year) + "12312300"
    }
    
    try:
        response = requests.get("https://web-api.tp.entsoe.eu/api",
                        params = params)
    except requests.exceptions.RequestException as e:
        raise SystemExit(e)
        
    return response