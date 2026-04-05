import os

from flask import Flask, render_template
from werkzeug.exceptions import HTTPException

from modules.module1 import module1
from modules.module2 import module2
from modules.module3 import module3
from modules.module4 import module4
from modules.module5 import module5
from modules.module6 import module6
from modules.module7 import module7
from modules.module8 import module8


def format_money(value, currency: str = "RUB") -> str:
    try:
        amount = float(value)
    except (TypeError, ValueError):
        return "—"

    formatted = f"{amount:,.2f}".replace(",", " ")
    return f"{formatted} {currency}"


def create_app() -> Flask:
    app = Flask(__name__)

    # Регистрация blueprint'ов
    app.register_blueprint(module1, url_prefix="/module1")
    app.register_blueprint(module2, url_prefix="/module2")
    app.register_blueprint(module3, url_prefix="/module3")
    app.register_blueprint(module4, url_prefix="/module4")
    app.register_blueprint(module5, url_prefix="/module5")
    app.register_blueprint(module6, url_prefix="/module6")
    app.register_blueprint(module7, url_prefix="/module7")
    app.register_blueprint(module8, url_prefix="/module8")

    app.jinja_env.filters["money"] = format_money

    @app.route("/")
    def index():
        return render_template("index.html")

    @app.errorhandler(404)
    def not_found(error):
        return render_template("index.html"), 404

    @app.errorhandler(500)
    def internal_error(error):
        return render_template("index.html"), 500

    @app.errorhandler(Exception)
    def handle_unexpected_error(error):
        # Пропускаем HTTP-исключения с корректными кодами
        if isinstance(error, HTTPException):
            return error
        return render_template("index.html"), 500

    return app


def _env_flag(name: str, default: bool = False) -> bool:
    value = os.getenv(name)
    if value is None:
        return default
    return value.strip().lower() in {"1", "true", "yes", "on"}


if __name__ == "__main__":
    app = create_app()

    debug_mode = _env_flag("FLASK_DEBUG", default=False)
    # host = os.getenv("FLASK_HOST", "193.41.142.253")
    host = os.getenv("FLASK_HOST", "127.0.0.1")
    port = int(os.getenv("FLASK_PORT", "5000"))

    app.run(host=host, port=port, debug=debug_mode)
