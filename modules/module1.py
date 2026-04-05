from flask import Blueprint, render_template, request

module1 = Blueprint('module1', __name__, template_folder='templates')

# Константа нормативного коэффициента эффективности (Eн = 0.2)
EN = 0.2

# Таблица 1: коэффициент реновации kp в зависимости от срока службы Тсл
KP_TABLE = {
    1: 1.0000,
    2: 0.4762,
    3: 0.3021,
    4: 0.2155,
    5: 0.1638,
    6: 0.1296,
    7: 0.1054,
    8: 0.0874,
    9: 0.0736,
    10: 0.0627,
    11: 0.0540,
    12: 0.0468,
    13: 0.0408,
    14: 0.0352,
    15: 0.0315
}

# Таблица 2: коэффициенты приведения a_t к расчётному году
ALPHA_PREV = {1: 1.100, 2: 1.210, 3: 1.331, 4: 1.464, 5: 1.611}   # предшествующие годы
ALPHA_NEXT = {1: 0.9091, 2: 0.8264, 3: 0.7513, 4: 0.6830, 5: 0.6209} # следующие годы
ALPHA_ZERO = 1.0

# ----------------------------------------------------------------------
# Главная страница модуля 1 (список доступных расчётов)
@module1.route('/')
def index():
    return render_template('module1/index.html')

# ----------------------------------------------------------------------
# Страница с таблицами 1 и 2 (справочная информация)
@module1.route('/tables')
def tables():
    return render_template('module1/tables.html',
                           kp_table=KP_TABLE,
                           alpha_prev=ALPHA_PREV,
                           alpha_next=ALPHA_NEXT)

# ----------------------------------------------------------------------
# Расчёт 1: Годовой экономический эффект (формула 1), коэффициент эффективности (2),
#           срок окупаемости (3). Все три показателя на одной странице.
@module1.route('/calc_eff', methods=['GET', 'POST'])
def calc_eff():
    result = {}
    if request.method == 'POST':
        try:
            P = float(request.form['profit'])          # годовая экономия (прирост прибыли)
            K = float(request.form['total_costs'])     # суммарные затраты (К)
            R = float(request.form['single_costs'])    # единовременные затраты (Р)
            # Годовой экономический эффект (1)
            E = P - K
            # Коэффициент эффективности (2)
            Ek = P / R if R != 0 else float('inf')
            # Срок окупаемости (3)
            T = R / P if P != 0 else float('inf')
            result = {
                'E': round(E, 2),
                'Ek': round(Ek, 4),
                'T': round(T, 2)
            }
        except (ValueError, ZeroDivisionError):
            result = {'error': 'Проверьте корректность введённых данных'}
    return render_template('module1/calc_eff.html', result=result)

# ----------------------------------------------------------------------
# Расчёт 2: Годовая экономия П по составляющим (формула 4)
@module1.route('/calc_economy', methods=['GET', 'POST'])
def calc_economy():
    result = {}
    if request.method == 'POST':
        try:
            P1 = float(request.form['p1'])
            P2 = float(request.form['p2'])
            P3 = float(request.form['p3'])
            DT = float(request.form['dt'])
            # П = (П1+П2+П3) * (1 + Eн * DT)
            total = (P1 + P2 + P3) * (1 + EN * DT)
            result = {'P_total': round(total, 2)}
        except ValueError:
            result = {'error': 'Введите числовые значения'}
    return render_template('module1/calc_economy.html', result=result)

# ----------------------------------------------------------------------
# Расчёт 3: Суммарные затраты на создание и внедрение К (формула 5) с выбором kp
@module1.route('/calc_total_costs', methods=['GET', 'POST'])
def calc_total_costs():
    result = {}
    if request.method == 'POST':
        try:
            I2 = float(request.form['i2'])          # годовые текущие издержки
            P_single = float(request.form['p_single'])  # единовременные затраты Р
            # Способ определения kp: либо из таблицы, либо расчёт по формуле (6)
            method = request.form.get('kp_method', 'table')
            if method == 'table':
                Tsl = int(request.form['tsl_table'])
                kp = KP_TABLE.get(Tsl, 0)
            else:
                Tsl = float(request.form['tsl_formula'])
                # формула (6): kp = Eн / ((1+Eн)^Tsl - 1)
                kp = EN / ((1 + EN) ** Tsl - 1) if (1 + EN) ** Tsl - 1 != 0 else float('inf')
            # K = I2 + (kp + Eн) * P
            K = I2 + (kp + EN) * P_single
            result = {
                'K': round(K, 2),
                'kp': round(kp, 4),
                'method': method
            }
        except (ValueError, ZeroDivisionError, KeyError):
            result = {'error': 'Проверьте введённые данные'}
    return render_template('module1/calc_total_costs.html',
                           kp_table=KP_TABLE, result=result)

# ----------------------------------------------------------------------
# Расчёт 4: Экономический эффект за расчётный период (формулы 7-9)
# Пользователь вводит данные для нескольких лет (год относительно расчётного,
# экономия П_t, затраты К_t). Для каждого года автоматически подбирается a_t.
@module1.route('/calc_period_effect', methods=['GET', 'POST'])
def calc_period_effect():
    result = {}
    if request.method == 'POST':
        try:
            years = []
            # ожидается, что поля названы year_0, profit_0, cost_0; year_1, ...
            i = 0
            P0_sum = 0
            K0_sum = 0
            while f'year_{i}' in request.form:
                year = int(request.form[f'year_{i}'])
                profit = float(request.form[f'profit_{i}'])
                cost = float(request.form[f'cost_{i}'])
                # определение a_t
                if year < 0:
                    alpha = ALPHA_PREV.get(abs(year), 1.0)
                elif year > 0:
                    alpha = ALPHA_NEXT.get(year, 1.0)
                else:
                    alpha = ALPHA_ZERO
                P0_sum += profit * alpha
                K0_sum += cost * alpha
                years.append({'year': year, 'profit': profit, 'cost': cost, 'alpha': round(alpha, 4)})
                i += 1
            omega = P0_sum - K0_sum
            result = {
                'P0': round(P0_sum, 2),
                'K0': round(K0_sum, 2),
                'omega': round(omega, 2),
                'years': years
            }
        except (ValueError, KeyError):
            result = {'error': 'Ошибка ввода данных. Проверьте правильность заполнения полей.'}
    return render_template('module1/calc_period_effect.html', result=result)

# ----------------------------------------------------------------------
# Расчёт 5: Предпроизводственные затраты (формулы 11 и 12)
@module1.route('/calc_preprod', methods=['GET', 'POST'])
def calc_preprod():
    result = {}
    if request.method == 'POST':
        try:
            method = request.form.get('preprod_method', 'components')
            if method == 'components':
                # по составляющим (11)
                P_ip = float(request.form['p_ip'])   # проектирование
                P_po = float(request.form['p_po'])   # программирование
                P_io = float(request.form['p_io'])   # информационное обеспечение
                P_bb = float(request.form['p_bb'])   # отладка и ввод
                P_ii = P_ip + P_po + P_io + P_bb
            else:
                # по смете через трудоёмкость (12)
                t_pr = float(request.form['t_pr'])   # приведённая трудоёмкость (чел.-дн.)
                C_d = float(request.form['c_d'])      # стоимость чел.-дн.
                P_ii = t_pr * C_d
            result = {'P_ii': round(P_ii, 2)}
        except ValueError:
            result = {'error': 'Введите числовые значения'}
    return render_template('module1/calc_preprod.html', result=result)

# ----------------------------------------------------------------------
# Расчёт 6: Капитальные затраты (формулы 13, 14)
@module1.route('/calc_capital', methods=['GET', 'POST'])
def calc_capital():
    result = {}
    if request.method == 'POST':
        try:
            P_ktc = float(request.form['p_ktc'])
            P_mont = float(request.form['p_mont'])
            P_inv = float(request.form['p_inv'])
            P_zd = float(request.form['p_zd'])
            P_os = float(request.form['p_os'])
            P_tr = float(request.form['p_tr'])
            P_sop = float(request.form['p_sop'])
            # расчёт остаточной стоимости высвобожденных средств (14)
            use_vysv = 'use_vysv' in request.form
            if use_vysv:
                P_v_first = float(request.form['p_v_first'])
                a = float(request.form['a_norm'])           # норма амортизации
                T_tehn = float(request.form['t_tehn'])      # срок эксплуатации
                P_vysv = P_v_first * (1 - a * T_tehn)
            else:
                P_vysv = 0
            # Pк = сумма всех - Pвысв (13)
            P_k = (P_ktc + P_mont + P_inv + P_zd + P_os + P_tr + P_sop) - P_vysv
            result = {
                'P_k': round(P_k, 2),
                'P_vysv': round(P_vysv, 2)
            }
        except ValueError:
            result = {'error': 'Проверьте введённые значения'}
    return render_template('module1/calc_capital.html', result=result)

# ----------------------------------------------------------------------
# Расчёт 7: Трудоёмкость и длительность разработки ПО (формулы 15–18)
@module1.route('/calc_software', methods=['GET', 'POST'])
def calc_software():
    result = {}
    if request.method == 'POST':
        try:
            n = float(request.form['n'])   # тысяч исходных команд
            # трудоёмкость t (чел.-мес.) (15)
            t = 3.6 * (n ** 1.2)
            # длительность T (мес.) (16)
            T = 2.5 * (t ** 0.32)
            # производительность труда (17)
            Pr = 1000 * n / t if t != 0 else 0
            # среднее число исполнителей (18)
            Ch = t / T if T != 0 else 0
            result = {
                't': round(t, 2),
                'T': round(T, 2),
                'Pr': round(Pr, 2),
                'Ch': round(Ch, 2)
            }
        except (ValueError, ZeroDivisionError):
            result = {'error': 'Некорректное значение n'}
    return render_template('module1/calc_software.html', result=result)

# ----------------------------------------------------------------------
# Расчёт 8: Текущие затраты на функционирование (формулы 19–21)
@module1.route('/calc_current', methods=['GET', 'POST'])
def calc_current():
    result = {}
    if request.method == 'POST':
        try:
            method = request.form.get('current_method', 'first')
            if method == 'first':
                # первый метод: Иг = Икса + Из (19)
                I_kts = float(request.form['i_kts'])
                I_son = float(request.form['i_son'])
                I_n = float(request.form['i_n'])
                I_z_eks = float(request.form['i_z_eks'])   # зарплата группы эксплуатации
                I_ksa = I_kts + I_son + I_n + I_z_eks
                I_z_spec = float(request.form['i_z_spec']) # зарплата специалистов
                I_g = I_ksa + I_z_spec
                result = {
                    'I_g': round(I_g, 2),
                    'method': 'Первый метод (через составляющие)'
                }
            else:
                # второй метод: Иг = сумма Иi + Исист (21)
                n_tasks = int(request.form['n_tasks'])
                sum_Ii = 0
                for i in range(n_tasks):
                    val = float(request.form[f'task_{i}'])
                    sum_Ii += val
                I_sist = float(request.form['i_sist'])
                I_g = sum_Ii + I_sist
                result = {
                    'I_g': round(I_g, 2),
                    'method': f'Второй метод (сумма по {n_tasks} задачам)'
                }
        except (ValueError, KeyError):
            result = {'error': 'Проверьте ввод данных'}
    return render_template('module1/calc_current.html', result=result)