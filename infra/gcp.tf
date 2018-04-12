provider "google" {
  credentials = "${file("account.json")}"
  project     = "misthos-173012"
  region      = "europe-west1"
}

resource "google_storage_bucket" "misthos-web-staging" {
  name          = "test.misthos.io"
  location      = "EU"
  storage_class = "MULTI_REGIONAL"

  website {
    main_page_suffix = "index.html"
  }
  cors {
    origin = ["*"]
    method = ["GET","OPTIONS"]
    response_header = ["content-type"]
  }
}

resource "google_storage_bucket_acl" "staging-acl" {
  bucket = "${google_storage_bucket.misthos-web-staging.name}"

  predefined_acl = "publicread"
  default_acl    = "publicread"
}
resource "google_storage_bucket_iam_member" "member" {
  bucket = "${google_storage_bucket.misthos-web-staging.name}"
  role        = "roles/storage.objectAdmin"
  member      = "serviceAccount:concourse@misthos-173012.iam.gserviceaccount.com"
}

resource "google_storage_bucket" "misthos-web-prod" {
  name          = "app.misthos.io"
  location      = "EU"
  storage_class = "MULTI_REGIONAL"

  website {
    main_page_suffix = "index.html"
  }
  cors {
    origin = ["*"]
    method = ["GET","OPTIONS"]
    response_header = ["content-type"]
  }
}

resource "google_storage_bucket_acl" "prod-acl" {
  bucket = "${google_storage_bucket.misthos-web-prod.name}"

  predefined_acl = "publicread"
  default_acl    = "publicread"
}
