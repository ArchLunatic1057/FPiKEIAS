import math

from flask import Blueprint, render_template, request

module8 = Blueprint("module8", __name__, template_folder="templates")

# Константы (на основе текста практической работы)
# Hyperion
HYPERION_PRICE_PER_SEAT = 700  # USD
HYPERION_SUPPORT = 154  # USD (на одного пользователя)

# Business Studio (цены в рублях)
BS_BASIC_PRICE = 44000  # руб. за лицензию
BS_ADVANCED_PRICE = 63000  # руб. за лицензию
BS_COCKPIT_PER_10 = 9800  # руб. за 10 лицензий Cockpit

# Для SAP и Oracle нет фиксированных цен; оставляем возможность ввести свои значения
SAP_DEFAULT_PRICE = 2500  # USD (пример)
ORACLE_DEFAULT_PRICE = 2000  # USD (пример)
USD_TO_RUB_DEFAULT = 95.0


def _to_int(value, default):
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def _to_float(value, default):
    try:
        return float(value)
    except (TypeError, ValueError):
        return default


@module8.route("/")
def index():
    return render_template("module8/index.html")


@module8.route("/calc", methods=["GET", "POST"])
def calc():
    defaults = {
        "seats": 10,
        "sap_price": SAP_DEFAULT_PRICE,
        "oracle_price": ORACLE_DEFAULT_PRICE,
        "bs_version": "basic",
        "use_cockpit": False,
        "usd_to_rub": USD_TO_RUB_DEFAULT,
    }

    # Чтобы форма сохраняла значения между запросами
    result = {"inputs": defaults}

    if request.method == "POST":
        try:
            seats = max(1, _to_int(request.form.get("seats"), defaults["seats"]))
            sap_price = max(
                0.0, _to_float(request.form.get("sap_price"), defaults["sap_price"])
            )
            oracle_price = max(
                0.0,
                _to_float(request.form.get("oracle_price"), defaults["oracle_price"]),
            )
            usd_to_rub = max(
                0.0, _to_float(request.form.get("usd_to_rub"), defaults["usd_to_rub"])
            )

            bs_version = request.form.get("bs_version", defaults["bs_version"])
            if bs_version not in {"basic", "advanced"}:
                bs_version = defaults["bs_version"]

            use_cockpit = "use_cockpit" in request.form

            inputs = {
                "seats": seats,
                "sap_price": sap_price,
                "oracle_price": oracle_price,
                "bs_version": bs_version,
                "use_cockpit": use_cockpit,
                "usd_to_rub": usd_to_rub,
            }

            # SAP / Oracle / Hyperion (USD)
            sap_total_usd = sap_price * seats
            oracle_total_usd = oracle_price * seats

            hyperion_licenses_usd = HYPERION_PRICE_PER_SEAT * seats
            hyperion_support_usd = HYPERION_SUPPORT * seats
            hyperion_total_usd = hyperion_licenses_usd + hyperion_support_usd

            # Business Studio (RUB)
            bs_price_per_seat_rub = (
                BS_BASIC_PRICE if bs_version == "basic" else BS_ADVANCED_PRICE
            )
            bs_licenses_total_rub = bs_price_per_seat_rub * seats

            cockpit_blocks = math.ceil(seats / 10) if use_cockpit else 0
            cockpit_cost_rub = cockpit_blocks * BS_COCKPIT_PER_10
            bs_total_rub = bs_licenses_total_rub + cockpit_cost_rub

            # Нормализация валют: приводим всё к RUB
            sap_total_rub = sap_total_usd * usd_to_rub
            oracle_total_rub = oracle_total_usd * usd_to_rub
            hyperion_total_rub = hyperion_total_usd * usd_to_rub

            # Дополнительно для наглядности: эквивалент Business Studio в USD
            bs_total_usd = (bs_total_rub / usd_to_rub) if usd_to_rub > 0 else None

            comparison_rub = {
                "SAP Strategic Enterprise Management (SEM-CPM)": sap_total_rub,
                "Oracle Balanced Scorecard": oracle_total_rub,
                "Hyperion Performance Scorecard": hyperion_total_rub,
                "Business Studio": bs_total_rub,
            }
            min_product = min(
                comparison_rub, key=lambda product: comparison_rub[product]
            )

            result = {
                "inputs": inputs,
                "seats": seats,
                "usd_to_rub": usd_to_rub,
                "sap": {
                    "price_per_seat_usd": round(sap_price, 2),
                    "total_usd": round(sap_total_usd, 2),
                    "total_rub": round(sap_total_rub, 2),
                },
                "oracle": {
                    "price_per_seat_usd": round(oracle_price, 2),
                    "total_usd": round(oracle_total_usd, 2),
                    "total_rub": round(oracle_total_rub, 2),
                },
                "hyperion": {
                    "price_per_seat_usd": HYPERION_PRICE_PER_SEAT,
                    "total_licenses_usd": round(hyperion_licenses_usd, 2),
                    "total_support_usd": round(hyperion_support_usd, 2),
                    "total_usd": round(hyperion_total_usd, 2),
                    "total_rub": round(hyperion_total_rub, 2),
                },
                "business_studio": {
                    "version": bs_version,
                    "price_per_seat_rub": bs_price_per_seat_rub,
                    "total_licenses_rub": round(bs_licenses_total_rub, 2),
                    "cockpit_blocks": cockpit_blocks,
                    "cockpit_cost_rub": round(cockpit_cost_rub, 2),
                    "total_rub": round(bs_total_rub, 2),
                    "total_usd": round(bs_total_usd, 2)
                    if bs_total_usd is not None
                    else None,
                },
                "comparison_rub": {k: round(v, 2) for k, v in comparison_rub.items()},
                "min_product": min_product,
            }
        except Exception:
            result = {
                "error": "Проверьте корректность введённых данных",
                "inputs": defaults,
            }

    return render_template("module8/calc.html", result=result)
