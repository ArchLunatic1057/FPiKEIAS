import math

from flask import Blueprint, render_template, request

module7 = Blueprint("module7", __name__, template_folder="templates")

# Константы
CMM_LICENSE_10 = 44000  # стоимость лицензии CMM на 10 рабочих мест, USD
CMM_PRICE_PER_SEAT = CMM_LICENSE_10 / 10  # 4400 USD за место

# IBM WebSphere Business Modeler
IBM_BASIC_PRICE = 1500  # USD за лицензию Basic (включает годовую поддержку)
IBM_ADVANCED_PRICE = 11500  # USD за лицензию Advanced (включает годовую поддержку)

# ARIS BSC
ARIS_PRICE = 2600  # EUR за лицензию
ARIS_SUPPORT_PERCENT = 0.22  # 22% от стоимости за годовую поддержку


def _to_int(value, default):
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


@module7.route("/")
def index():
    return render_template("module7/index.html")


@module7.route("/calc", methods=["GET", "POST"])
def calc():
    defaults = {
        "seats": 10,
        "ibm_version": "basic",
    }

    # Для корректного отображения формы даже до первого POST
    result = {"inputs": defaults}

    if request.method == "POST":
        try:
            seats = max(1, _to_int(request.form.get("seats"), defaults["seats"]))
            ibm_version = request.form.get("ibm_version", defaults["ibm_version"])
            if ibm_version not in {"basic", "advanced"}:
                ibm_version = defaults["ibm_version"]

            # Выбор версии IBM
            if ibm_version == "basic":
                ibm_price_per_seat = IBM_BASIC_PRICE
                ibm_support_included = True
                ibm_support_cost = 0.0
            else:
                ibm_price_per_seat = IBM_ADVANCED_PRICE
                ibm_support_included = True
                ibm_support_cost = 0.0

            # ARIS
            aris_price_total = ARIS_PRICE * seats
            aris_support = aris_price_total * ARIS_SUPPORT_PERCENT
            aris_total = aris_price_total + aris_support

            # CMM продаётся блоками по 10 лицензий
            blocks = math.ceil(seats / 10)
            cmm_total = blocks * CMM_LICENSE_10

            # IBM линейно
            ibm_total = ibm_price_per_seat * seats

            result = {
                "inputs": {
                    "seats": seats,
                    "ibm_version": ibm_version,
                },
                "seats": seats,
                "cmm": {
                    "price_per_seat": round(CMM_PRICE_PER_SEAT, 2),
                    "licensing_blocks": blocks,
                    "total": round(cmm_total, 2),
                    "support_included": True,
                    "support_cost": 0.0,
                    "total_with_support": round(cmm_total, 2),
                },
                "ibm": {
                    "version": ibm_version,
                    "price_per_seat": round(ibm_price_per_seat, 2),
                    "total": round(ibm_total, 2),
                    "support_included": ibm_support_included,
                    "support_cost": round(ibm_support_cost, 2),
                    "total_with_support": round(ibm_total, 2),
                },
                "aris": {
                    "price_per_seat": round(ARIS_PRICE, 2),
                    "total_licenses": round(aris_price_total, 2),
                    "support_percent": ARIS_SUPPORT_PERCENT * 100,
                    "support_cost": round(aris_support, 2),
                    "total_with_support": round(aris_total, 2),
                },
            }

            # Минимальная стоимость (как и ранее, без приведения EUR/USD)
            prices = {
                "Cognos Metrics Manager": result["cmm"]["total_with_support"],
                f"IBM WebSphere Business Modeler ({ibm_version})": result["ibm"][
                    "total_with_support"
                ],
                "ARIS BSC": result["aris"]["total_with_support"],
            }
            result["min_product"] = min(prices, key=lambda product: prices[product])

        except Exception:
            result = {
                "error": "Проверьте корректность введённых данных",
                "inputs": defaults,
            }

    return render_template("module7/calc.html", result=result)
