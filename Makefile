PROJECT = iam
PROJECT_DESCRIPTION = Identity and Access Management (IAM)
PROJECT_VERSION = 0.0.0

DEPS = jose ojson

dep_jose = git git://github.com/potatosalad/erlang-jose.git master
dep_ojson = git git://github.com/potatosalad/erlang-ojson.git master

include erlang.mk
