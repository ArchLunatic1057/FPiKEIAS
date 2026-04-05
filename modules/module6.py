from flask import Blueprint, render_template, request

module6 = Blueprint("module6", __name__, template_folder="templates")

# Константы
DAYS_PER_MONTH = 21
HOURS_PER_DAY = 8
NOM_DAYS_YEAR = 258

# Значения по умолчанию в "человеческих" процентах (как в форме)
REPAIR_PERCENT_DEFAULT = 2.0
SOCIAL_RATE_PERCENT_DEFAULT = 34.0
OVERHEAD_RATE_PERCENT_DEFAULT = 50.0
MATERIALS_PERCENT_DEFAULT = 1.5
MAINTENANCE_PERCENT_DEFAULT = 2.5
LIQUIDATION_PERCENT_DEFAULT = 5.0

SERVICE_LIFE = 6
POWER = 0.45
LOAD_FACTOR = 0.8
ELECTRIC_PRICE = 6.6
MONTHLY_SALARY_DEFAULT = 10850
EQUIPMENT_COST_DEFAULT = 26400

# Таблица 1 – коэффициенты расчёта трудоёмкости (язык высокого/низкого уровня)
COMPLEXITY_TABLE = {
    "high": {
        1: {"A": 1.38, "B": 1.26, "V": 1.15, "G": 0.69},
        2: {"A": 1.22, "B": 1.30, "V": 1.09, "G": 0.65},
        3: {"A": 1.20, "B": 1.10, "V": 1.00, "G": 0.60},
    },
    "low": {
        1: {"A": 1.58, "B": 1.45, "V": 1.32, "G": 0.79},
        2: {"A": 1.49, "B": 1.37, "V": 1.24, "G": 0.74},
        3: {"A": 1.38, "B": 1.26, "V": 1.15, "G": 0.69},
    },
}

# Коэффициент недостаточности описания B для каждой группы
B_TABLE = {1: 1.2, 2: 1.35, 3: 1.5}

# Таблица 2 – оценка времени подготовки описания задачи
DESC_TIME_TABLE = {
    100: (10, 15, 20),
    500: (20, 35, 50),
    1000: (25, 50, 75),
    1500: (30, 60, 90),
    2000: (40, 70, 100),
    2500: (50, 80, 110),
    5000: (70, 110, 150),
    10000: (100, 150, 200),
}

# Таблица 3 – коэффициенты квалификации программиста
QUAL_TABLE = {
    "до 2 лет": 0.8,
    "2-3 года": 1.0,
    "3-5 лет": 1.1,
    "5-7 лет": 1.3,
    "более 7 лет": 1.5,
}


@module6.route("/")
def index():
    return render_template("module6/index.html")


@module6.route("/tables")
def tables():
    return render_template("module6/tables.html")


@module6.route("/calc", methods=["GET", "POST"])
def calc():
    result = None
    if request.method == "POST":
        try:
            data = collect_form_data(request)
            result = compute_cost(data)
        except Exception as e:
            result = {"error": str(e)}
    return render_template("module6/calc.html", result=result)


def _percent_to_ratio(value_percent: float) -> float:
    """Преобразует проценты (например, 34) в долю (0.34)."""
    return value_percent / 100.0


def collect_form_data(request):
    """Собирает все данные из формы. Процентные поля принимаются в процентах."""
    data = {}

    # 1. Основные параметры
    data["q"] = int(request.form.get("q", 1086))
    data["p"] = float(request.form.get("p", 0.06))
    data["language"] = request.form.get("language", "high")
    data["complexity_group"] = int(request.form.get("complexity_group", 1))
    data["novelty"] = request.form.get("novelty", "A")

    data["c"] = COMPLEXITY_TABLE[data["language"]][data["complexity_group"]][
        data["novelty"]
    ]
    data["B"] = B_TABLE[data["complexity_group"]]

    exp_level = request.form.get("exp_level", "до 2 лет")
    data["exp_level"] = exp_level
    data["k"] = QUAL_TABLE[exp_level]

    # 2. Зарплата и оборудование
    data["monthly_salary"] = float(
        request.form.get("monthly_salary", MONTHLY_SALARY_DEFAULT)
    )
    data["equipment_cost"] = float(
        request.form.get("equipment_cost", EQUIPMENT_COST_DEFAULT)
    )
    data["power"] = float(request.form.get("power", POWER))
    data["load_factor"] = float(request.form.get("load_factor", LOAD_FACTOR))
    data["electric_price"] = float(request.form.get("electric_price", ELECTRIC_PRICE))

    # 3. Нормативы
    data["days_per_month"] = int(request.form.get("days_per_month", DAYS_PER_MONTH))
    data["hours_per_day"] = int(request.form.get("hours_per_day", HOURS_PER_DAY))
    data["nom_days_year"] = int(request.form.get("nom_days_year", NOM_DAYS_YEAR))
    data["repair_percent"] = float(
        request.form.get("repair_percent", REPAIR_PERCENT_DEFAULT)
    )
    data["social_rate_percent"] = float(
        request.form.get("social_rate", SOCIAL_RATE_PERCENT_DEFAULT)
    )
    data["overhead_rate_percent"] = float(
        request.form.get("overhead_rate", OVERHEAD_RATE_PERCENT_DEFAULT)
    )
    data["materials_percent"] = float(
        request.form.get("materials_percent", MATERIALS_PERCENT_DEFAULT)
    )
    data["maintenance_percent"] = float(
        request.form.get("maintenance_percent", MAINTENANCE_PERCENT_DEFAULT)
    )
    data["liquidation_percent"] = float(
        request.form.get("liquidation_percent", LIQUIDATION_PERCENT_DEFAULT)
    )
    data["service_life"] = int(request.form.get("service_life", SERVICE_LIFE))

    return data


def compute_cost(data):
    """Выполняет расчёт себестоимости по формулам."""
    q = data["q"]
    c = data["c"]
    p = data["p"]
    B = data["B"]
    k = data["k"]

    monthly_salary = data["monthly_salary"]
    equipment_cost = data["equipment_cost"]
    power = data["power"]
    load_factor = data["load_factor"]
    electric_price = data["electric_price"]

    days_per_month = data["days_per_month"]
    hours_per_day = data["hours_per_day"]
    nom_days_year = data["nom_days_year"]
    repair_percent = data["repair_percent"]
    service_life = data["service_life"]

    # Проценты -> доли
    social_rate = _percent_to_ratio(data["social_rate_percent"])
    overhead_rate = _percent_to_ratio(data["overhead_rate_percent"])
    materials_rate = _percent_to_ratio(data["materials_percent"])
    maintenance_rate = _percent_to_ratio(data["maintenance_percent"])
    liquidation_rate = _percent_to_ratio(data["liquidation_percent"])

    # ---------- 1. Базовый показатель Q ----------
    Q = q * c * (1 + p)

    # ---------- 2. t_pp (интерполяция по таблице 2) ----------
    q_list = sorted(DESC_TIME_TABLE.keys())
    tmin, tnv, tmax = 0.0, 0.0, 0.0

    if q <= q_list[0]:
        tmin, tnv, tmax = DESC_TIME_TABLE[q_list[0]]
    elif q >= q_list[-1]:
        tmin, tnv, tmax = DESC_TIME_TABLE[q_list[-1]]
    else:
        for i in range(len(q_list) - 1):
            if q_list[i] <= q <= q_list[i + 1]:
                q1, q2 = q_list[i], q_list[i + 1]
                tmin1, tnv1, tmax1 = DESC_TIME_TABLE[q1]
                tmin2, tnv2, tmax2 = DESC_TIME_TABLE[q2]

                tmin = tmin1 + (tmin2 - tmin1) * (q - q1) / (q2 - q1)
                tnv = tnv1 + (tnv2 - tnv1) * (q - q1) / (q2 - q1)
                tmax = tmax1 + (tmax2 - tmax1) * (q - q1) / (q2 - q1)
                break

    t_pp = (tmin + 4 * tnv + tmax) / 6

    # ---------- 3. t_u ----------
    denom_u = 160**k
    t_u = (Q * B) / denom_u

    # ---------- 4-5. t_a, t_n ----------
    denom_ab = 45**k
    t_a = Q / denom_ab
    t_n = Q / denom_ab

    # ---------- 6. t_o ----------
    denom_o = 9**k
    t_o = Q / denom_o

    # ---------- 7. t_d ----------
    t_r = Q / denom_ab
    t_d = 1.75 * t_r

    # ---------- 8. t_z ----------
    t_z = t_pp + t_u + t_a + t_n + t_o + t_d

    # ---------- 9. ЗП ----------
    base_salary = (t_z / (days_per_month * hours_per_day)) * monthly_salary
    add_salary = 0.2 * base_salary
    fzp = base_salary + add_salary

    # ---------- 10. Социальные и накладные ----------
    social = social_rate * fzp
    overhead = overhead_rate * fzp

    # ---------- 11. Материалы и ТО ----------
    materials = materials_rate * equipment_cost
    maintenance = maintenance_rate * equipment_cost

    # ---------- 12. Амортизация ----------
    liquidation = liquidation_rate * equipment_cost
    amort_rate = (equipment_cost - liquidation) / (service_life * equipment_cost) * 100
    amort_annual = equipment_cost * amort_rate / 100

    # ---------- 13. Электроэнергия ----------
    eff_fund = nom_days_year * hours_per_day * (1 - repair_percent / 100.0)
    electricity_annual = power * load_factor * eff_fund * electric_price

    # ---------- 14. Коррекция по времени использования ----------
    t_comp = t_n + t_o + t_d
    w = t_comp / eff_fund if eff_fund else 0.0

    electricity_adj = electricity_annual * w
    maintenance_adj = maintenance * w
    amort_adj = amort_annual * w
    total_operating = electricity_adj + maintenance_adj + amort_adj

    # ---------- 15. Итог ----------
    total_cost = fzp + social + overhead + total_operating + materials

    return {
        "q": q,
        "c": round(c, 4),
        "p": p,
        "B": B,
        "k": k,
        "Q": round(Q, 2),
        "t_pp": round(t_pp, 2),
        "t_u": round(t_u, 2),
        "t_a": round(t_a, 2),
        "t_n": round(t_n, 2),
        "t_o": round(t_o, 2),
        "t_d": round(t_d, 2),
        "t_z": round(t_z, 2),
        "base_salary": round(base_salary, 2),
        "add_salary": round(add_salary, 2),
        "fzp": round(fzp, 2),
        "social": round(social, 2),
        "overhead": round(overhead, 2),
        "materials": round(materials, 2),
        "maintenance": round(maintenance, 2),
        "amort_annual": round(amort_annual, 2),
        "electricity_annual": round(electricity_annual, 2),
        "eff_fund": round(eff_fund, 2),
        "t_comp": round(t_comp, 2),
        "w": round(w, 4),
        "electricity_adj": round(electricity_adj, 2),
        "maintenance_adj": round(maintenance_adj, 2),
        "amort_adj": round(amort_adj, 2),
        "total_operating": round(total_operating, 2),
        "total_cost": round(total_cost, 2),
        # Для повторного отображения формы/контекста
        "language": data["language"],
        "complexity_group": data["complexity_group"],
        "novelty": data["novelty"],
        "exp_level": data["exp_level"],
        "monthly_salary": monthly_salary,
        "equipment_cost": equipment_cost,
        # Проценты как вводил пользователь (в процентах)
        "social_rate_percent": data["social_rate_percent"],
        "overhead_rate_percent": data["overhead_rate_percent"],
        "materials_percent": data["materials_percent"],
        "maintenance_percent": data["maintenance_percent"],
        "liquidation_percent": data["liquidation_percent"],
        "repair_percent": data["repair_percent"],
    }
