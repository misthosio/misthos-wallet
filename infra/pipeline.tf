resource "google_storage_bucket" "misthos-artifact-cache" {
  name          = "artifact-cache"
  location      = "EU"
}

resource "google_storage_bucket_iam_member" "cache-concourse" {
  bucket = "${google_storage_bucket.misthos-artifact-cache.name}"
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:concourse@misthos-173012.iam.gserviceaccount.com"
}
