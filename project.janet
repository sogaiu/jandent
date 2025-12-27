(declare-project
  :name "jandent"
  :url "https://github.com/sogaiu/jandent"
  :repo "git+https://github.com/sogaiu/jandent")

(declare-source
  :source ["jandent"])

(declare-executable
  :name "jindt"
  :entry "jandent/jindt.janet"
  :install true)

(declare-binscript
  :main "jandent.janet"
  :is-janet true)

