package common

object Constants {
    // Bootstrap params
    val BOOTSTRAP_IP_ADDRESS: String = "localhost"
    val BOOTSTRAP_SERVER_PORT: Int = 9001

    // Network params
    val PORT_RANGE_START: Int = 20000
    val PORT_RANGE_WIDTH: Int = 2000

    // Tokens
    val GAME_CNT_LEEWAY_FACTOR: Int = 1
    val TOKEN_RETRIES_CNT = 5
    val TOKEN_RETRY_MS = 200

    // Game
    val GAME_BUCKET_SIZE = 10

    // Other
    val MAX_JOIN_ATTEMPTS = 3
    val HEALTHCHECK_INTERVAL_MS = 250
    val EPS = 0.001
}
