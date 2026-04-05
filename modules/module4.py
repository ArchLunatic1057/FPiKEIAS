from flask import Blueprint, render_template, request

module4 = Blueprint('module4', __name__, template_folder='templates')

# Константы (значения по умолчанию из примера)
EN_DEFAULT = 0.2
KZ_DEFAULT = 1.0
FM_DEFAULT = 166          # месячный фонд времени, ч (не используется напрямую)
FPV_DEFAULT = 1020        # годовой фонд полезного времени одного технолога, ч
KVN_DEFAULT = 1.2         # коэффициент выполнения норм
K_PREMIUM = 1.7
K_REGION = 1.0
K_ADD = 1.08
K_SOCIAL = 1.34
K_OVERHEAD = 1.6
NA = 0.2                  # норма амортизации
H_MATER = 0.01            # 1% от стоимости ПЭВМ на материалы
H_REPAIR = 0.035          # 3.5% на ремонт
H_OTHER = 0.005           # 0.5% прочие
T_BASE = 0.17             # длительность цикла ТПП в базовом варианте, лет
D_COEFF = 0.54            # коэффициент сокращения цикла

@module4.route('/')
def index():
    return render_template('module4/index.html')

@module4.route('/calc', methods=['GET', 'POST'])
def calc():
    result = None
    if request.method == 'POST':
        try:
            data = collect_form_data(request)
            result = compute_efficiency(data)
        except Exception as e:
            result = {'error': str(e)}
    return render_template('module4/calc.html', result=result)

def collect_form_data(request):
    """Собирает все данные из формы"""
    data = {}
    # Количество ТП в год
    data['H'] = float(request.form.get('H', 200))
    # Трудоёмкость базового варианта (в примере 26,38 часа)
    data['T_base'] = float(request.form.get('T_base', 26.38))
    # Трудоёмкость с ИАС (в примере 5,38 часа)
    data['T_ias'] = float(request.form.get('T_ias', 5.38))
    # Годовой фонд полезного времени технолога
    data['Fpv'] = float(request.form.get('Fpv', FPV_DEFAULT))
    # Коэффициент выполнения норм
    data['Kvn'] = float(request.form.get('Kvn', KVN_DEFAULT))
    # Среднечасовая зарплата технолога (в примере 60 руб./час)
    data['C_hour'] = float(request.form.get('C_hour', 60))
    # Коэффициенты оплаты труда
    data['Kp'] = float(request.form.get('Kp', K_PREMIUM))
    data['Kr'] = float(request.form.get('Kr', K_REGION))
    data['Kd'] = float(request.form.get('Kd', K_ADD))
    data['Ks'] = float(request.form.get('Ks', K_SOCIAL))
    data['Kn'] = float(request.form.get('Kn', K_OVERHEAD))
    # Эксплуатационные расходы
    data['Zsr'] = float(request.form.get('Zsr', 10000))          # среднемесячная зарплата обслуживающего персонала
    data['Ch'] = int(request.form.get('Ch', 2))                  # численность обслуживающего персонала
    data['Na'] = float(request.form.get('Na', NA))               # норма амортизации
    data['C_perv'] = float(request.form.get('C_perv', 20000))    # стоимость ПЭВМ
    data['Kvt'] = float(request.form.get('Kvt', 76500))          # доп. капитальные затраты на СВТ
    data['Kstr'] = float(request.form.get('Kstr', 26000))        # капитальные затраты на строительство
    data['Fg'] = float(request.form.get('Fg', FPV_DEFAULT))      # годовой фонд работы ПЭВМ (ч)
    data['C_el'] = float(request.form.get('C_el', 6.6))          # стоимость электроэнергии, руб/кВт·ч
    data['P_el'] = float(request.form.get('P_el', 0.5))          # потребляемая мощность, кВт
    data['Kz'] = float(request.form.get('Kz', KZ_DEFAULT))       # коэффициент загрузки технических средств
    data['Kpr'] = float(request.form.get('Kpr', 13000))          # предпроизводственные затраты
    # Параметры для экономии от сокращения цикла
    data['T_tspp'] = float(request.form.get('T_tspp', T_BASE))   # длительность цикла ТПП в базовом варианте, лет
    data['d'] = float(request.form.get('d', D_COEFF))            # коэффициент сокращения цикла
    data['En'] = float(request.form.get('En', EN_DEFAULT))       # нормативный коэффициент эффективности
    return data

def compute_efficiency(data):
    """Выполняет все расчёты по формулам"""
    H = data['H']
    T_base = data['T_base']
    T_ias = data['T_ias']
    Fpv = data['Fpv']
    Kvn = data['Kvn']
    C_hour = data['C_hour']
    Kp = data['Kp']
    Kr = data['Kr']
    Kd = data['Kd']
    Ks = data['Ks']
    Kn = data['Kn']
    Zsr = data['Zsr']
    Ch = data['Ch']
    Na = data['Na']
    C_perv = data['C_perv']
    Kvt = data['Kvt']
    Kstr = data['Kstr']
    Fg = data['Fg']
    C_el = data['C_el']
    P_el = data['P_el']
    Kz = data['Kz']
    Kpr = data['Kpr']
    T_tspp = data['T_tspp']
    d = data['d']
    En = data['En']

    # 1. Коэффициент g
    g = T_ias / T_base

    # 2. Трудоёмкости
    Q1 = H * T_base
    Q2 = Q1 * g
    delta_Q = Q1 - Q2

    # 3. Численность условно-высвобождаемых технологов
    N_vysv = delta_Q / (Fpv * Kvn)

    # 4. Снижение себестоимости проектирования ΔCпр
    #   Сначала вычисляем W – годовые расходы на эксплуатацию ИАС
    #   Фонд оплаты труда обслуживающего персонала
    Zpl = Zsr * Ch * 12 * Kd * Ks
    #   Амортизация
    Zamort = Na * (C_perv + Kvt + Kstr)
    #   Затраты на электроэнергию
    Z_el = Fg * C_el * P_el
    #   Затраты на материалы (1% от стоимости ПЭВМ)
    Z_mater = 0.01 * C_perv
    #   Затраты на ремонт (3,5% от стоимости ПЭВМ)
    Z_rem = 0.035 * C_perv
    #   Накладные расходы (к основной зарплате технолога)
    Z_nakl = Zsr * 12 * Kn
    #   Прочие затраты (0,5% от стоимости ПЭВМ)
    Z_proch = 0.005 * C_perv
    #   Суммарные годовые эксплуатационные расходы
    W = Zpl + Zamort + Z_el + Z_mater + Z_rem + Z_nakl + Z_proch

    #   Снижение себестоимости
    delta_Cpr = delta_Q * C_hour * Kp * Kr * Kd * Ks - W

    # 5. Изменение единовременных затрат ΔKпр
    delta_Kpr = (Kvt + Kstr) * Kz + Kpr

    # 6. Экономия от сокращения цикла Σμ
    Sigma_mu = En * delta_Cpr * T_tspp * d

    # 7. Годовой экономический эффект Σгод
    Sigma_god = delta_Cpr - delta_Kpr * En + Sigma_mu

    # 8. Срок окупаемости дополнительных затрат
    if delta_Cpr + Sigma_mu != 0:
        T_ok = delta_Kpr / (delta_Cpr + Sigma_mu)
    else:
        T_ok = float('inf')

    # 9. Расчётный коэффициент эффективности
    denominator = C_perv + Kvt + Kstr
    if denominator != 0:
        E_pr = Sigma_god / denominator
    else:
        E_pr = 0

    # Подготовка результата
    result = {
        'g': round(g, 4),
        'Q1': round(Q1, 2),
        'Q2': round(Q2, 2),
        'delta_Q': round(delta_Q, 2),
        'N_vysv': round(N_vysv, 2),
        'Zpl': round(Zpl, 2),
        'Zamort': round(Zamort, 2),
        'Z_el': round(Z_el, 2),
        'Z_mater': round(Z_mater, 2),
        'Z_rem': round(Z_rem, 2),
        'Z_nakl': round(Z_nakl, 2),
        'Z_proch': round(Z_proch, 2),
        'W': round(W, 2),
        'delta_Cpr': round(delta_Cpr, 2),
        'delta_Kpr': round(delta_Kpr, 2),
        'Sigma_mu': round(Sigma_mu, 2),
        'Sigma_god': round(Sigma_god, 2),
        'T_ok': round(T_ok, 2) if T_ok != float('inf') else '∞',
        'E_pr': round(E_pr, 4),
        'En': En
    }
    return result