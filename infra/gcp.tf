provider "google" {
  credentials = "${file("account.json")}"
  project     = "misthos-173012"
  region      = "europe-west1"
}

resource "google_storage_bucket" "misthos-web-staging" {
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

resource "google_storage_bucket_iam_member" "staging-concourse" {
  bucket = "${google_storage_bucket.misthos-web-staging.name}"
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:concourse@misthos-173012.iam.gserviceaccount.com"
}

resource "google_storage_bucket_iam_member" "staging-public" {
  bucket = "${google_storage_bucket.misthos-web-staging.name}"
  role   = "roles/storage.objectViewer"
  member = "allUsers"
}

resource "google_storage_bucket" "misthos-web-prod" {
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
  bucket = "${google_storage_bucket.misthos-web-prod.name}"
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:concourse@misthos-173012.iam.gserviceaccount.com"
}

resource "google_storage_bucket_iam_member" "prod-public" {
  bucket = "${google_storage_bucket.misthos-web-prod.name}"
  role   = "roles/storage.objectViewer"
  member = "allUsers"
}
