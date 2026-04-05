from flask import Blueprint, render_template, request

module5 = Blueprint('module5', __name__, template_folder='templates')

# Константы
DAYS_PER_MONTH = 23      # количество рабочих дней в месяце
SOCIAL_RATE_DEFAULT = 34 # % отчислений на социальные нужды
OTHER_RATE_DEFAULT = 0   # прочие расходы, руб. (можно ввести отдельно)

@module5.route('/')
def index():
    return render_template('module5/index.html')

@module5.route('/tables')
def tables():
    return render_template('module5/tables.html')

@module5.route('/calc', methods=['GET', 'POST'])
def calc():
    result = None
    if request.method == 'POST':
        try:
            data = collect_form_data(request)
            result = compute_cost(data)
        except Exception as e:
            result = {'error': str(e)}
    return render_template('module5/calc.html', result=result)

def collect_form_data(request):
    """Собирает все данные из формы"""
    data = {}

    # 1. Материальные затраты (таблица 1)
    materials = []
    for i in range(1, 10):  # до 9 позиций
        name = request.form.get(f'mat_name_{i}')
        if name:
            qty = float(request.form.get(f'mat_qty_{i}', 0))
            price = float(request.form.get(f'mat_price_{i}', 0))
            materials.append({'name': name, 'qty': qty, 'price': price})
    data['materials'] = materials

    # 2. Затраты на труд (таблица 2)
    stages = []
    for i in range(1, 20):  # до 19 этапов
        stage_name = request.form.get(f'stage_name_{i}')
        if stage_name:
            positions = []
            for j in range(1, 4):
                pos_name = request.form.get(f'stage_{i}_pos_{j}_name')
                if pos_name:
                    count = int(request.form.get(f'stage_{i}_pos_{j}_count', 0))
                    days = float(request.form.get(f'stage_{i}_pos_{j}_days', 0))
                    salary = float(request.form.get(f'stage_{i}_pos_{j}_salary', 0))
                    positions.append({
                        'name': pos_name,
                        'count': count,
                        'days': days,
                        'salary': salary
                    })
            if positions:
                stages.append({
                    'name': stage_name,
                    'positions': positions
                })
    data['stages'] = stages

    # Коэффициенты оплаты труда
    data['add_wage_percent'] = float(request.form.get('add_wage_percent', 10))
    data['social_rate'] = float(request.form.get('social_rate', SOCIAL_RATE_DEFAULT))

    # 3. Параметры для расчёта себестоимости 1 машино-часа
    data['tech_salary'] = float(request.form.get('tech_salary', 240000))
    data['pc_balance'] = float(request.form.get('pc_balance', 500000))
    data['depreciation_rate'] = float(request.form.get('depreciation_rate', 20))
    data['total_power'] = float(request.form.get('total_power', 5))
    data['annual_hours'] = float(request.form.get('annual_hours', 2000))
    data['electric_price'] = float(request.form.get('electric_price', 6.6))
    data['prevent_rate'] = float(request.form.get('prevent_rate', 2))
    data['other_prod_rate'] = float(request.form.get('other_prod_rate', 30))
    data['eff_fund'] = float(request.form.get('eff_fund', 1800))
    data['pc_time_total'] = float(request.form.get('pc_time_total', 500))

    # 4. Накладные (косвенные) затраты
    data['indirect_total'] = float(request.form.get('indirect_total', 2000000))
    data['total_payroll'] = float(request.form.get('total_payroll', 5000000))

    # 5. Прочие расходы
    data['other_expenses'] = float(request.form.get('other_expenses', 0))

    return data

def compute_cost(data):
    """Выполняет все расчёты по формулам"""
    materials = data['materials']
    stages = data['stages']
    add_wage_percent = data['add_wage_percent']
    social_rate = data['social_rate']

    # ---------- 1. Прямые затраты на материалы ----------
    material_total = sum(m['qty'] * m['price'] for m in materials)

    # ---------- 2. Прямые затраты на труд ----------
    # Основная заработная плата (формула 1)
    base_salary_total = 0
    for stage in stages:
        for pos in stage['positions']:
            salary_one = (pos['days'] / DAYS_PER_MONTH) * pos['salary']
            base_salary_total += salary_one * pos['count']
    # Дополнительная заработная плата и отчисления (формула 2)
    labor_total = base_salary_total * (1 + add_wage_percent / 100) * (1 + social_rate / 100)

    # ---------- 3. Затраты на содержание и эксплуатацию ПЭВМ ----------
    # 3.1 Годовые затраты на эксплуатацию всех ПЭВМ
    # Зарплата обслуживающего персонала (уже включает начисления)
    tech_salary = data['tech_salary']
    # Амортизация
    amort = data['pc_balance'] * data['depreciation_rate'] / 100
    # Затраты на электроэнергию
    electricity = data['total_power'] * data['annual_hours'] * data['electric_price']
    # Расходы на профилактику
    prevent = data['pc_balance'] * data['prevent_rate'] / 100
    # Прочие производственные расходы (30% от основной зарплаты обслуживающего персонала)
    # Вычисляем основную зарплату (без начислений и доплат)
    # Если tech_salary уже содержит начисления, то основная = tech_salary / ((1+add_wage/100)*(1+social/100))
    if (1 + add_wage_percent/100) * (1 + social_rate/100) != 0:
        base_tech_salary = tech_salary / ((1 + add_wage_percent/100) * (1 + social_rate/100))
    else:
        base_tech_salary = tech_salary
    other_prod = base_tech_salary * data['other_prod_rate'] / 100
    # Суммарные годовые затраты
    total_annual_cost = tech_salary + amort + electricity + prevent + other_prod
    # 3.2 Себестоимость 1 машино-часа работы ПЭВМ (формула 3)
    if data['eff_fund'] != 0:
        cost_per_hour = total_annual_cost / data['eff_fund']
    else:
        cost_per_hour = 0
    # 3.3 Затраты на эксплуатацию ПЭВМ для данного продукта (формула 4)
    pc_cost = cost_per_hour * data['pc_time_total']

    # ---------- 4. Накладные расходы ----------
    # Фонд оплаты труда по проекту (основная + дополнительная, без отчислений)
    project_payroll = base_salary_total * (1 + add_wage_percent / 100)
    if data['total_payroll'] != 0:
        k = project_payroll / data['total_payroll']
    else:
        k = 0
    indirect_cost = data['indirect_total'] * k

    # 5. Прочие расходы
    other_cost = data['other_expenses']

    # Итоговая себестоимость
    total_cost = material_total + labor_total + pc_cost + indirect_cost + other_cost

    # Формируем результат
    result = {
        'material_total': round(material_total, 2),
        'labor_base': round(base_salary_total, 2),
        'labor_total': round(labor_total, 2),
        'pc_cost': round(pc_cost, 2),
        'cost_per_hour': round(cost_per_hour, 2),
        'indirect_total': round(indirect_cost, 2),
        'other_cost': round(other_cost, 2),
        'total_cost': round(total_cost, 2),
        'materials': materials,
        'stages': stages,
        'add_wage_percent': add_wage_percent,
        'k': round(k, 4),
        'project_payroll': round(project_payroll, 2)
    }
    return result