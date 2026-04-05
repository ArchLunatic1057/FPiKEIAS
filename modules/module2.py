from flask import Blueprint, render_template, request

module2 = Blueprint('module2', __name__, template_folder='templates')

# Константы
EN = 0.15
DAYS_IN_MONTH = 24
ALPHA_NEXT = {1: 0.9091, 2: 0.8264, 3: 0.7513, 4: 0.6830, 5: 0.6209}
ALPHA_ZERO = 1.0

@module2.route('/')
def index():
    return render_template('module2/index.html')

@module2.route('/tables')
def tables():
    return render_template('module2/tables.html')

@module2.route('/calc_arm', methods=['GET', 'POST'])
def calc_arm():
    result = None
    defaults = {
        'N_z': 50,
        'tz_baz': 20,
        'tz_proj': 0.15,
        'N_d': 200,
        'td_baz': 3,
        'td_proj': 0.1,
        'Cm': 150,
        'I_kts': 10307.5,
        'Iz_spec': 12890,
        'Cmm': 33,
        'Cs': 67,
        't_bd': 4,
        't_vv': 4,
        'salary_secretary_month': 6700,       # 5000+1700 из таблицы
        'n': 1.0,
        'Cpr': 200,
        'delta_T': 2,
        'En': EN,
        'years': 3
    }

    if request.method == 'POST':
        try:
            # Сбор данных из формы
            N_z = float(request.form.get('N_z', defaults['N_z']))
            tz_baz = float(request.form.get('tz_baz', defaults['tz_baz']))
            tz_proj = float(request.form.get('tz_proj', defaults['tz_proj']))
            N_d = float(request.form.get('N_d', defaults['N_d']))
            td_baz = float(request.form.get('td_baz', defaults['td_baz']))
            td_proj = float(request.form.get('td_proj', defaults['td_proj']))
            Cm = float(request.form.get('Cm', defaults['Cm']))
            I_kts = float(request.form.get('I_kts', defaults['I_kts']))
            Iz_spec = float(request.form.get('Iz_spec', defaults['Iz_spec']))
            Cmm = float(request.form.get('Cmm', defaults['Cmm']))
            Cs = float(request.form.get('Cs', defaults['Cs']))
            t_bd = float(request.form.get('t_bd', defaults['t_bd']))
            t_vv = float(request.form.get('t_vv', defaults['t_vv']))
            salary_secretary_month = float(request.form.get('salary_secretary_month', defaults['salary_secretary_month']))
            n = float(request.form.get('n', defaults['n']))
            Cpr = float(request.form.get('Cpr', defaults['Cpr']))
            delta_T = float(request.form.get('delta_T', defaults['delta_T']))
            En = float(request.form.get('En', defaults['En']))
            years = int(request.form.get('years', defaults['years']))

            # ------------------------------------------------------------------
            # 1. Единовременные затраты P (формулы 1-5)
            # Трудоёмкость разработки ПО (чел.-мес.) – формула (3) из методички
            t = 3.6 * (n ** 1.2)
            T_n = 2.5 * (t ** 0.32)                 # длительность разработки, мес.
            # Затраты на программирование
            P_no = Cpr * T_n * DAYS_IN_MONTH
            # Затраты на информационное обеспечение и ввод
            P_io = Cm * t_bd
            P_ve = Cm * t_vv
            # Итого единовременные затраты
            P = P_no + P_io + P_ve

            # ------------------------------------------------------------------
            # 2. Текущие затраты на функционирование
            I_g = I_kts + Iz_spec * 12               # годовые текущие затраты
            # Суммарные текущие затраты за years лет с приведением к первому году
            alpha_sum = ALPHA_ZERO
            for i in range(1, years):
                alpha_sum += ALPHA_NEXT.get(i, 0)
            I_total = I_g * alpha_sum

            # ------------------------------------------------------------------
            # 3. Суммарные затраты на создание и функционирование
            K2 = P + I_g                            # за первый год
            K_total = P + I_total                   # за весь период

            # ------------------------------------------------------------------
            # 4. Расчёт экономии
            # П1 – от сокращения штатной единицы секретаря (годовая)
            P1 = salary_secretary_month * 12
            # П2 – от сокращения сроков выполнения задач (формула 7)
            # В примере: П2 = Cc * t2 * N2 - Cm * 0.15 * N2
            # Здесь tz_baz = 20, N_z = 50, сокращение времени = 0.15 (по условию)
            # Но по логике, после внедрения АРМ время на задачу сократилось с 20 чел.-час до 0.15 маш.-час.
            # Поэтому экономия = (стоимость ручного труда) - (стоимость машинного времени)
            # Стоимость ручного труда: Cs * tz_baz * N_z
            # Стоимость машинного времени: Cm * tz_proj * N_z
            P2 = Cs * tz_baz * N_z - Cm * tz_proj * N_z
            # П3 – от сокращения времени печати документов (формула 8)
            # Аналогично: ручной труд: Cmm * td_baz * N_d
            # Машинное время: Cm * td_proj * N_d
            P3 = Cmm * td_baz * N_d - Cm * td_proj * N_d

            # Годовая прибыль (формула 9)
            P_year = (P1 + P2 + P3) * (1 + En * delta_T)

            # Экономия за весь период (формула 10)
            P0 = P_year * alpha_sum

            # ------------------------------------------------------------------
            # 5. Экономический эффект
            effect_year = P_year - K2
            effect_total = P0 - K_total

            # ------------------------------------------------------------------
            # 6. Коэффициент эффективности и срок окупаемости
            if (P_year - I_g) != 0:
                Ek = (P_year - I_g) / P
                T_ok = P / (P_year - I_g)
            else:
                Ek = float('inf')
                T_ok = float('inf')

            result = {
                # Исходные параметры (для отображения)
                'N_z': N_z, 'tz_baz': tz_baz, 'tz_proj': tz_proj,
                'N_d': N_d, 'td_baz': td_baz, 'td_proj': td_proj,
                'Cm': Cm, 'I_kts': I_kts, 'Iz_spec': Iz_spec,
                'Cmm': Cmm, 'Cs': Cs, 't_bd': t_bd, 't_vv': t_vv,
                'salary_secretary_month': salary_secretary_month,
                'n': n, 'Cpr': Cpr, 'delta_T': delta_T, 'En': En,
                # Расчётные
                't': round(t, 2),
                'T_n': round(T_n, 2),
                'P_no': round(P_no, 2),
                'P_io': round(P_io, 2),
                'P_ve': round(P_ve, 2),
                'P': round(P, 2),
                'I_g': round(I_g, 2),
                'alpha_sum': round(alpha_sum, 4),
                'I_total': round(I_total, 2),
                'K2': round(K2, 2),
                'K_total': round(K_total, 2),
                'P1': round(P1, 2),
                'P2': round(P2, 2),
                'P3': round(P3, 2),
                'P_year': round(P_year, 2),
                'P0': round(P0, 2),
                'effect_year': round(effect_year, 2),
                'effect_total': round(effect_total, 2),
                'Ek': round(Ek, 4) if Ek != float('inf') else '∞',
                'T_ok': round(T_ok, 2) if T_ok != float('inf') else '∞'
            }
        except Exception as e:
            result = {'error': str(e)}
    else:
        result = {'defaults': defaults}

    return render_template('module2/calc_arm.html', result=result)