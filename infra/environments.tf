provider "google" {
  credentials = "${file("account.json")}"
  project     = "misthos-173012"
  region      = "europe-west1"
}

resource "google_storage_bucket" "misthos-web-staging" {
  name          = "web-staging.misthos.io"
  location      = "EU"
  storage_class = "MULTI_REGIONAL"

  website {
    main_page_suffix = "index.html"
    not_found_page = "index.html"
  }
}

resource "google_storage_bucket_iam_member" "web-staging-concourse" {
  bucket = "${google_storage_bucket.misthos-web-staging.name}"
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:concourse@misthos-173012.iam.gserviceaccount.com"
}

resource "google_storage_bucket_iam_member" "web-staging-public" {
  bucket = "${google_storage_bucket.misthos-web-staging.name}"
  role   = "roles/storage.objectViewer"
  member = "allUsers"
}

resource "google_storage_bucket" "misthos-app-staging" {
  name          = "staging.misthos.io"
  location      = "EU"
  storage_class = "MULTI_REGIONAL"

  website {
    main_page_suffix = "index.html"
    not_found_page = "index.html"
  }

  cors {
    origin          = ["*"]
    method          = ["GET", "OPTIONS"]
    response_header = ["content-type"]
  }
}

resource "google_storage_bucket_iam_member" "staging-concourse" {
  bucket = "${google_storage_bucket.misthos-app-staging.name}"
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:concourse@misthos-173012.iam.gserviceaccount.com"
}

resource "google_storage_bucket_iam_member" "staging-public" {
  bucket = "${google_storage_bucket.misthos-app-staging.name}"
  role   = "roles/storage.objectViewer"
  member = "allUsers"
}

resource "google_storage_bucket" "misthos-app-testnet" {
  name          = "testnet.misthos.io"
  location      = "EU"
  storage_class = "MULTI_REGIONAL"

  website {
    main_page_suffix = "index.html"
    not_found_page = "index.html"
  }

  cors {
    origin          = ["*"]
    method          = ["GET", "OPTIONS"]
    response_header = ["content-type"]
  }
}

resource "google_storage_bucket_iam_member" "testnet-concourse" {
  bucket = "${google_storage_bucket.misthos-app-testnet.name}"
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:concourse@misthos-173012.iam.gserviceaccount.com"
}

resource "google_storage_bucket_iam_member" "testnet-public" {
  bucket = "${google_storage_bucket.misthos-app-testnet.name}"
  role   = "roles/storage.objectViewer"
  member = "allUsers"
}

resource "google_storage_bucket" "misthos-web-testnet" {
  name          = "web-testnet.misthos.io"
  location      = "EU"
  storage_class = "MULTI_REGIONAL"

  website {
    main_page_suffix = "index.html"
    not_found_page = "index.html"
  }
}

resource "google_storage_bucket_iam_member" "web-testnet-concourse" {
  bucket = "${google_storage_bucket.misthos-web-testnet.name}"
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:concourse@misthos-173012.iam.gserviceaccount.com"
}

resource "google_storage_bucket_iam_member" "web-testnet-public" {
  bucket = "${google_storage_bucket.misthos-web-testnet.name}"
  role   = "roles/storage.objectViewer"
  member = "allUsers"
}

resource "google_storage_bucket" "misthos-app-prod" {
  name          = "app.misthos.io"
  location      = "EU"
  storage_class = "MULTI_REGIONAL"

  website {
    main_page_suffix = "index.html"
    not_found_page = "index.html"
  }

  cors {
    origin          = ["*"]
    method          = ["GET", "OPTIONS"]
    response_header = ["content-type"]
  }
}

resource "google_storage_bucket_iam_member" "prod-concourse" {
  bucket = "${google_storage_bucket.misthos-app-prod.name}"
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:concourse@misthos-173012.iam.gserviceaccount.com"
}

resource "google_storage_bucket_iam_member" "prod-public" {
  bucket = "${google_storage_bucket.misthos-app-prod.name}"
  role   = "roles/storage.objectViewer"
  member = "allUsers"
}

resource "google_storage_bucket" "misthos-web-prod" {
  name          = "www.misthos.io"
  location      = "EU"
  storage_class = "MULTI_REGIONAL"

  website {
    main_page_suffix = "index.html"
    not_found_page = "index.html"
  }
}

resource "google_storage_bucket_iam_member" "web-prod-concourse" {
  bucket = "${google_storage_bucket.misthos-web-prod.name}"
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:concourse@misthos-173012.iam.gserviceaccount.com"
}

resource "google_storage_bucket_iam_member" "web-prod-public" {
  bucket = "${google_storage_bucket.misthos-web-prod.name}"
  role   = "roles/storage.objectViewer"
  member = "allUsers"
}
