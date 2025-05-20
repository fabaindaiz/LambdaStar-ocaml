(lang dune 2.9)

(name lambda_star)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lambda_star)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml
   (>= 4.10))
  dune
  alcotest
  containers
  )
 (tags
  (topics "to describe" your project)))