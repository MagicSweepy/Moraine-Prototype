## Serialization Part in Morphism API

> [!IMPORTANT]\
> This repository is only for experimental and doesn't mean it can work normally in dependencies.

Scala Implementation of Serialization System in **Morphism API**.

### Cheatsheet for some replaced dependencies

We want to minimize experimental costs, so not fully include existed dependencies in **Minecraft**,
instead of it, we use Scala dependencies by default.

| Lib          | MC Lib          |
|--------------|-----------------|
| ujson        | gson            |
| scala native | guava, fastutil |