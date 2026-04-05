from flask import Blueprint, render_template, request

module3 = Blueprint("module3", __name__, template_folder="templates")

# ----------------------------------------------------------------------
# Константы
FM = 166  # месячный фонд времени, ч (стр. 6)
FN = 2000  # годовой номинальный фонд времени, ч (стр. 8, 13)
K1_DEFAULT = 1.3  # премии 30% (стр. 6)
K2_DEFAULT = 1.12  # дополнительная зарплата 12% (стр. 6)
K3_DEFAULT = 1.34  # социальные начисления 34% (стр. 6)
K4_DEFAULT = 1.6  # накладные расходы 160% (стр. 10)
H_AM_DEFAULT = 20  # норма амортизации, % (стр. 8, можно 20…25)
H_TR_DEFAULT = 5  # доля затрат на обслуживание и ремонт, % (стр. 8)
EN_ELECTRIC_DEFAULT = 6.0  # стоимость электроэнергии, руб/кВт·ч (стр. 8)
H_TAX = 20  # налог на прибыль, % (стр. 14)
EN_DEFAULT = 0.2  # нормативный коэффициент эффективности (стр. 6)


# ----------------------------------------------------------------------
@module3.route("/")
def index():
    return render_template("module3/index.html")


# ----------------------------------------------------------------------
@module3.route("/calc", methods=["GET", "POST"])
def calc():
    result = None
    if request.method == "POST":
        try:
            # ---------- 1. Сбор данных из формы ----------
            data = collect_form_data(request)
            # ---------- 2. Расчёты ----------
            result = compute_efficiency(data)
        except Exception as e:
            result = {"error": str(e)}
    return render_template("module3/calc.html", result=result)


# ----------------------------------------------------------------------
def collect_form_data(request):
    """Собирает все данные из формы (полный набор)"""
    data = {}

    # Общие параметры
    data["N_plan"] = float(request.form.get("N_plan", 50))
    data["En"] = float(request.form.get("En", EN_DEFAULT))
    data["k1"] = float(request.form.get("k1", K1_DEFAULT))
    data["k2"] = float(request.form.get("k2", K2_DEFAULT))
    data["k3"] = float(request.form.get("k3", K3_DEFAULT))
    data["k4"] = float(request.form.get("k4", K4_DEFAULT))
    data["ha"] = float(request.form.get("ha", H_AM_DEFAULT))
    data["htr"] = float(request.form.get("htr", H_TR_DEFAULT))
    data["en_electric"] = float(request.form.get("en_electric", EN_ELECTRIC_DEFAULT))
    data["kz_bas"] = float(request.form.get("kz_bas", 1.0))
    data["kz_proj"] = float(request.form.get("kz_proj", 1.0))
    data["Ek"] = float(request.form.get("Ek", 0))
    data["Esop"] = float(request.form.get("Esop", 0))
    data["N_vyear"] = float(request.form.get("N_vyear", 0))

    # ---- Базовый вариант ----
    data["bas"] = {
        "designers_count": int(request.form.get("bas_designers_count", 0)),
        "designers_labor": float(request.form.get("bas_designers_labor", 0)),
        "designers_salary": float(request.form.get("bas_designers_salary", 0)),
        "technologists_count": int(request.form.get("bas_technologists_count", 0)),
        "technologists_labor": float(request.form.get("bas_technologists_labor", 0)),
        "technologists_salary": float(request.form.get("bas_technologists_salary", 0)),
        "other_count": int(request.form.get("bas_other_count", 0)),
        "other_labor": float(request.form.get("bas_other_labor", 0)),
        "other_salary": float(request.form.get("bas_other_salary", 0)),
        "tech": [],  # техника (будет заполнена ниже)
    }
    # Технические средства (до 5 штук для простоты)
    for i in range(1, 6):
        name = request.form.get(f"bas_tech_{i}_name")
        if name:
            data["bas"]["tech"].append(
                {
                    "name": name,
                    "power": float(request.form.get(f"bas_tech_{i}_power", 0)),
                    "cost": float(request.form.get(f"bas_tech_{i}_cost", 0)),
                    "eta": float(request.form.get(f"bas_tech_{i}_eta", 0)),
                }
            )
    # Эксплуатационные расходы W (могут быть введены напрямую, но если техника есть – рассчитываем)
    data["bas"]["W_input"] = float(request.form.get("bas_W", 0))
    # Единовременные затраты Кео (могут быть введены напрямую)
    data["bas"]["Keo_input"] = float(request.form.get("bas_Keo", 0))
    # Составляющие Кео (если нужно, но пока оставим только итоговую)

    # ---- Проектный вариант ----
    data["proj"] = {
        "designers_count": int(request.form.get("proj_designers_count", 0)),
        "designers_labor": float(request.form.get("proj_designers_labor", 0)),
        "designers_salary": float(request.form.get("proj_designers_salary", 0)),
        "technologists_count": int(request.form.get("proj_technologists_count", 0)),
        "technologists_labor": float(request.form.get("proj_technologists_labor", 0)),
        "technologists_salary": float(request.form.get("proj_technologists_salary", 0)),
        "other_count": int(request.form.get("proj_other_count", 0)),
        "other_labor": float(request.form.get("proj_other_labor", 0)),
        "other_salary": float(request.form.get("proj_other_salary", 0)),
        "tech": [],
    }
    for i in range(1, 6):
        name = request.form.get(f"proj_tech_{i}_name")
        if name:
            data["proj"]["tech"].append(
                {
                    "name": name,
                    "power": float(request.form.get(f"proj_tech_{i}_power", 0)),
                    "cost": float(request.form.get(f"proj_tech_{i}_cost", 0)),
                    "eta": float(request.form.get(f"proj_tech_{i}_eta", 0)),
                }
            )
    data["proj"]["W_input"] = float(request.form.get("proj_W", 0))
    data["proj"]["Keo_input"] = float(request.form.get("proj_Keo", 0))

    # Составляющие единовременных затрат для проектного варианта (формулы 10-14)
    data["proj"]["ling_cost"] = float(request.form.get("ling_cost", 0))
    data["proj"]["license_cost"] = float(request.form.get("license_cost", 0))
    data["proj"]["dev_t"] = float(request.form.get("dev_t", 0))
    data["proj"]["dev_r"] = int(request.form.get("dev_r", 0))
    data["proj"]["dev_salary"] = float(request.form.get("dev_salary", 0))
    data["proj"]["info_cost"] = float(request.form.get("info_cost", 0))
    data["proj"]["method_cost"] = float(request.form.get("method_cost", 0))
    data["proj"]["other_percent"] = float(request.form.get("other_percent", 10))

    # Показатели качества (ΔPj и ΔИЭj)
    data["dp_resources"] = []
    for i in range(1, 6):
        name = request.form.get(f"dp_name_{i}")
        if name:
            data["dp_resources"].append(
                {
                    "name": name,
                    "delta": float(request.form.get(f"dp_delta_{i}", 0)),
                    "price": float(request.form.get(f"dp_price_{i}", 0)),
                }
            )
    data["di_resources"] = []
    for i in range(1, 6):
        name = request.form.get(f"di_name_{i}")
        if name:
            data["di_resources"].append(
                {
                    "name": name,
                    "delta": float(request.form.get(f"di_delta_{i}", 0)),
                    "price": float(request.form.get(f"di_price_{i}", 0)),
                }
            )

    return data


# ----------------------------------------------------------------------
def compute_efficiency(data):
    """Выполняет все расчёты по формулам"""
    # Распаковка параметров
    N_plan = data["N_plan"]
    En = data["En"]
    k1, k2, k3, k4 = data["k1"], data["k2"], data["k3"], data["k4"]
    ha, htr = data["ha"], data["htr"]
    en_electric = data["en_electric"]
    kz_bas, kz_proj = data["kz_bas"], data["kz_proj"]
    Ek_input = data["Ek"]
    Esop = data["Esop"]
    N_vyear = data["N_vyear"]

    # ---------- Функции расчёта ----------
    def calc_zopl(spec):
        """Затраты на оплату труда по формуле (3)"""
        total = (
            spec["designers_count"] * spec["designers_labor"] * spec["designers_salary"]
            + spec["technologists_count"]
            * spec["technologists_labor"]
            * spec["technologists_salary"]
            + spec["other_count"] * spec["other_labor"] * spec["other_salary"]
        ) / FM
        return total * k1 * k2 * k3

    def calc_w(tech, direct_w):
        """Эксплуатационные расходы W по формулам (4)-(9)"""
        if tech:
            Pa = sum(t["cost"] * ha / 100 for t in tech)
            Pen = FN * en_electric * sum(t["power"] * t["eta"] for t in tech)
            Ptr = sum(t["cost"] * htr / 100 for t in tech)
            Pm = 0  # можно добавить отдельный ввод
            Ppr = 0.1 * (Pa + Pen + Ptr + Pm)  # прочие 10% (стр. 9)
            return Pa + Pen + Ptr + Pm + Ppr
        else:
            return direct_w  # если техники нет, используем введённое значение W

    def calc_keo(
        tech,
        ling,
        license_cost,
        dev_t,
        dev_r,
        dev_salary,
        info,
        method,
        other_percent,
        direct_keo,
    ):
        """Единовременные затраты Кео по формулам (10)-(14)"""
        if direct_keo != 0:
            return direct_keo  # если введено вручную, используем его
        # Техника
        tech_sum = sum(t["cost"] for t in tech)
        # ПО: лицензионное + разработанное
        if dev_t > 0:
            po_dev = (
                dev_t * dev_r * (dev_salary / FM) * (k1 * k2 * k3 + k4)
            )  # формула (12)
        else:
            po_dev = 0
        po_cost = license_cost + po_dev
        total = tech_sum + po_cost + info + method + ling
        other = total * other_percent / 100
        return total + other

    def calc_es(spec_bas, spec_proj, w_bas, w_proj, kz_bas, kz_proj, N_plan):
        """Годовая экономия текущих затрат Эс по формуле (16)"""
        z_bas = calc_zopl(spec_bas)
        z_proj = calc_zopl(spec_proj)
        return N_plan * (z_bas - z_proj) + (w_bas * kz_bas - w_proj * kz_proj)

    def calc_ncrit(spec_bas, spec_proj, w_bas, w_proj, keo_bas, keo_proj, En):
        """Критическое число Nкр (вывод формулы на стр. 11)"""
        z_bas = calc_zopl(spec_bas)
        z_proj = calc_zopl(spec_proj)
        diff_z = z_bas - z_proj
        diff_w = w_proj - w_bas
        diff_keo = keo_proj - keo_bas
        numerator = diff_w + En * diff_keo
        if diff_z != 0:
            return numerator / diff_z
        else:
            return float("inf") if numerator > 0 else 0

    def calc_ek():
        """Экономия от повышения качества Эк (формулы 18, 19 + ввод пользователя)"""
        e_iz = sum(dp["delta"] * dp["price"] for dp in data["dp_resources"]) * N_vyear
        e_e = sum(di["delta"] * di["price"] for di in data["di_resources"]) * N_vyear
        # Учитываем как рассчитанную, так и введённую пользователем экономию от качества
        return Ek_input + e_iz + e_e

    # ---------- Выполнение расчётов для базового варианта ----------
    spec_bas = data["bas"]
    w_bas = calc_w(spec_bas["tech"], spec_bas["W_input"])
    keo_bas = calc_keo(spec_bas["tech"], 0, 0, 0, 0, 0, 0, 0, 0, spec_bas["Keo_input"])
    z_bas = calc_zopl(spec_bas)

    # ---------- Проектный вариант ----------
    spec_proj = data["proj"]
    w_proj = calc_w(spec_proj["tech"], spec_proj["W_input"])
    keo_proj = calc_keo(
        spec_proj["tech"],
        spec_proj["ling_cost"],
        spec_proj["license_cost"],
        spec_proj["dev_t"],
        spec_proj["dev_r"],
        spec_proj["dev_salary"],
        spec_proj["info_cost"],
        spec_proj["method_cost"],
        spec_proj["other_percent"],
        spec_proj["Keo_input"],
    )
    z_proj = calc_zopl(spec_proj)

    # Трудовые затраты (чел.-ч)
    labor_bas = (
        spec_bas["designers_count"] * spec_bas["designers_labor"]
        + spec_bas["technologists_count"] * spec_bas["technologists_labor"]
        + spec_bas["other_count"] * spec_bas["other_labor"]
    )
    labor_proj = (
        spec_proj["designers_count"] * spec_proj["designers_labor"]
        + spec_proj["technologists_count"] * spec_proj["technologists_labor"]
        + spec_proj["other_count"] * spec_proj["other_labor"]
    )

    # Численность специалистов
    count_bas = (
        spec_bas["designers_count"]
        + spec_bas["technologists_count"]
        + spec_bas["other_count"]
    )
    count_proj = (
        spec_proj["designers_count"]
        + spec_proj["technologists_count"]
        + spec_proj["other_count"]
    )

    # Текущие затраты (Зопл + W)
    current_bas = z_bas + w_bas
    current_proj = z_proj + w_proj

    # Приведённые затраты (формула 2)
    pz_bas = z_bas + (w_bas / N_plan) + En * (keo_bas / N_plan)
    pz_proj = z_proj + (w_proj / N_plan) + En * (keo_proj / N_plan)

    # Критическое число
    n_crit = calc_ncrit(spec_bas, spec_proj, w_bas, w_proj, keo_bas, keo_proj, En)

    # Годовая экономия текущих затрат
    es = calc_es(spec_bas, spec_proj, w_bas, w_proj, kz_bas, kz_proj, N_plan)

    # Экономия от повышения качества
    ek = calc_ek()

    # Годовой экономический эффект (формула 15)
    delta_keo = keo_proj * kz_proj - keo_bas * kz_bas
    e_god = es + ek + Esop - En * delta_keo

    # Период окупаемости дополнительных единовременных затрат (формула 20)
    if delta_keo > 0 and (es + ek + Esop) != 0:
        t_ok = delta_keo / (es + ek + Esop) * (1 - H_TAX / 100)
    else:
        t_ok = 0

    # Проценты проектного варианта к базовому
    def percent(base, proj):
        if base != 0:
            return round(proj / base * 100, 2)
        return "-"

    # Формирование результата
    result = {
        "N_plan": N_plan,
        "bas": {
            "labor": labor_bas,
            "count": count_bas,
            "current": current_bas,
            "keo": keo_bas,
            "pz": pz_bas,
        },
        "proj": {
            "labor": labor_proj,
            "count": count_proj,
            "current": current_proj,
            "keo": keo_proj,
            "pz": pz_proj,
        },
        "n_crit": round(n_crit, 2) if n_crit != float("inf") else "∞",
        "e_god": round(e_god, 2),
        "t_ok": round(t_ok, 2),
        "percent": {
            "labor": percent(labor_bas, labor_proj),
            "count": percent(count_bas, count_proj),
            "current": percent(current_bas, current_proj),
            "keo": percent(keo_bas, keo_proj),
            "pz": percent(pz_bas, pz_proj),
        },
    }
    return result
