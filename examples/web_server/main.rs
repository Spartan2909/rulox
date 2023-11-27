mod framework;

use rulox::prelude::*;
use rulox::LoxError;

async fn server() -> Result<(), LoxError> {
    let framework = framework::get_module("examples/web_server/templates/**/*.html")?;

    lox! {
        async fun index(request) {
            var name = request.GET.get_or("name", "World");
            var context = framework.Context({ "name": name });
            context.filter("times_five", fun(value, _args) return value * 5;);
            return framework.render("index.html", request, context);
        }

        async fun json(_request) {
            return framework.JsonResponse({ "name": "James", "age": 32 });
        }

        async fun error(_request) {
            throw "this is a bad handler";
        }

        async fun handler_404(_request) {
            return "<h1>This page doesn't exist.</h1> <h3>Maybe get the URL right next time, moron :)</h3>";
        }

        var router = framework.Router()
            .route("/", framework.get(index))
            .route("/json", framework.get(json))
            .route("/error", framework.get(error))
            .handler_404(handler_404);

        framework.Server("127.0.0.1:8000").serve(router).await;
    }

    Ok(())
}

#[tokio::main]
async fn main() {
    if let Err(error) = server().await {
        eprintln!("{error}");
    }
}
