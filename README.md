# Frenv

** Frenv is generic Rust data structure loader from environment variables.

## Frenv in action

```rust
use frenv::FromEnv;

#[derive(FromEnv, Debug)]
struct Config {
    // Loads from HOST env var.
    host: String,
    // Loads from PORT env var.
    port: u16,
    redis_config: RedisConfig,
    rabbit_config: RabbitMqConfig,
}

#[derive(FromEnv, Debug)]
#[frenv(prefix = "REDIS")]
struct RedisConfig {
    // Loads from REDIS_HOST env var.
    host: String,
    // Loads from REDIS_PORT env var.
    port: u16,
}

#[derive(FromEnv, Debug)]
#[frenv(prefix = "RABBITMQ")]
struct RabbitMqConfig {
    // Loads from RABBITMQ_NAME env var, but sets default value from fn if is missing.
    #[frenv(default = "Self::default_name")]
    name: String,
    // Loads from RABBITMQ_SENTINEL_HOST env var.
    #[frenv(prefix = "SENTINEL")]
    host: String,
    // Loads from RABBITMQ_SENTINEL_PORT env var.
    #[frenv(prefix = "SENTINEL")]
    port: u16,
    // Loads from RABBITMQ_SENTINEL_PASSWORD env var.
    #[frenv(prefix = "SENTINEL")]
    password: String,
}

impl RabbitMqConfig {
    fn default_name() -> String {
        "rabbit".to_string()
    }
}

fn main() {
    std::env::set_var("HOST", "localhost");
    std::env::set_var("PORT", 8000);
    std::env::set_var("REDIS_HOST", "localhost");
    std::env::set_var("REDIS_PORT", 8001);
    std::env::set_var("RABBITMQ_NAME", "localhost");
    // std::env::set_var("RABBITMQ_SENTINEL_NAME", "name"); // doest not set so it loads from Default
    std::env::set_var("RABBITMQ_SENTINEL_PORT", 8002);
    std::env::set_var("RABBITMQ_SENTINEL_PASSWORD", "secretpassword");

    let config = Config::from_env().expect("failed to load config from env");

    // prints loaded = Config { host: "localhost", port: 8000, ... }
    println!("loaded = {:?}", config);
}
```
