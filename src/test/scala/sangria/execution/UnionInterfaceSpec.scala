package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.schema._
import sangria.util.{GraphQlSupport, AwaitSupport}


class UnionInterfaceSpec extends WordSpec with Matchers with AwaitSupport with GraphQlSupport {
  trait Named {
    def name: Option[String]
  }

  case class Dog(name: Option[String], barks: Option[Boolean]) extends Named
  case class Cat(name: Option[String], meows: Option[Boolean]) extends Named
  case class Person(name: Option[String], pets: Option[List[Option[AnyRef]]], friends: Option[List[Option[Named]]]) extends Named

  val NamedType = InterfaceType("Named", fields[Unit, Named](
    Field("name", OptionType(StringType))(_.value.name)))

  val DogType = ObjectType("Dog", interfaces = interfaces[Unit, Dog](NamedType), fields = fields[Unit, Dog](
    Field("barks", OptionType(BooleanType))(_.value.barks)))

  val CatType = ObjectType("Cat", interfaces = interfaces[Unit, Cat](NamedType), fields = fields[Unit, Cat](
    Field("meows", OptionType(BooleanType))(_.value.meows)))

  val PetType = UnionType[Unit]("Pet", types = DogType :: CatType :: Nil)

  val PersonType = ObjectType("Person", interfaces = interfaces[Unit, Person](NamedType), fields = fields[Unit, Person](
    Field("pets", OptionType(ListType(OptionType(PetType))))(_.value.pets),
    Field("friends", OptionType(ListType(OptionType(NamedType))))(_.value.friends)))
  
  val TestSchema = Schema(PersonType)

  val garfield = Cat(Some("Garfield"), Some(false))
  val odie = Dog(Some("Odie"), Some(true))
  val liz = Person(Some("Liz"), None, None)
  val bob = Person(Some("Bob"), Some(List(Some(garfield), Some(odie))), Some(List(Some(liz), Some(odie))))

  val schema = Schema(PersonType)

  "Execute: Union and intersection types" should {
    "introspect on union and intersection types" in check(
      (),
      """
        {
          Named: __type(name: "Named") {
            kind
            name
            fields { name }
            interfaces { name }
            possibleTypes { name }
            enumValues { name }
            inputFields { name }
          }
          Pet: __type(name: "Pet") {
            kind
            name
            fields { name }
            interfaces { name }
            possibleTypes { name }
            enumValues { name }
            inputFields { name }
          }
        }
      """,
      Map(
        "data" -> Map(
          "Named" -> Map(
            "kind" -> "INTERFACE",
            "name" -> "Named",
            "fields" -> List(
              Map("name" -> "name")
            ),
            "interfaces" -> null,
            "possibleTypes" -> List(
              Map("name" -> "Cat"),
              Map("name" -> "Dog"),
              Map("name" -> "Person")
            ),
            "enumValues" -> null,
            "inputFields" -> null
          ),
          "Pet" -> Map(
            "kind" -> "UNION",
            "name" -> "Pet",
            "fields" -> null,
            "interfaces" -> null,
            "possibleTypes" -> List(
              Map("name" -> "Dog"),
              Map("name" -> "Cat")
            ),
            "enumValues" -> null,
            "inputFields" -> null
          )
        )
      )
    )

    "executes using union types" in check(
      bob,
      """
       {
         __typename
         name
         pets {
           __typename
           name
           barks
           meows
         }
       }
      """,
      Map(
        "data" -> Map(
          "__typename" -> "Person",
          "name" -> "Bob",
          "pets" -> List(
            Map("__typename" -> "Cat", "name" -> "Garfield", "meows" -> false),
            Map("__typename" -> "Dog", "name" -> "Odie", "barks" -> true)
          )
        )
      )
    )

    "executes union types with inline fragments" in check(
      bob,
      """
        {
          __typename
          name
          pets {
            __typename
            ... on Dog {
              name
              barks
            }
            ... on Cat {
              name
              meows
            }
          }
        }
      """,
      Map(
        "data" -> Map(
          "__typename" -> "Person",
          "name" -> "Bob",
          "pets" -> List(
            Map("__typename" -> "Cat", "name" -> "Garfield", "meows" -> false),
            Map("__typename" -> "Dog", "name" -> "Odie", "barks" -> true)
          )
        )
      )
    )

    "executes using interface types" in check(
      bob,
      """
        {
          __typename
          name
          friends {
            __typename
            name
            barks
            meows
          }
        }
      """,
      Map(
        "data" -> Map(
          "__typename" -> "Person",
          "name" -> "Bob",
          "friends" -> List(
            Map("__typename" -> "Person", "name" -> "Liz"),
            Map("__typename" -> "Dog", "name" -> "Odie", "barks" -> true)
          )
        )
      )
    )

    "executes interface types with inline fragments" in check(
      bob,
      """
        {
          __typename
          name
          friends {
            __typename
            name
            ... on Dog {
              barks
            }
            ... on Cat {
              meows
            }
          }
        }
      """,
      Map(
        "data" -> Map(
          "__typename" -> "Person",
          "name" -> "Bob",
          "friends" -> List(
            Map("__typename" -> "Person", "name" -> "Liz"),
            Map("__typename" -> "Dog", "name" -> "Odie", "barks" -> true)
          )
        )
      )
    )

    "allows fragment conditions to be abstract types" in check(
      bob,
      """
        {
          __typename
          name
          pets { ...PetFields }
          friends { ...FriendFields }
        }

        fragment PetFields on Pet {
          __typename
          ... on Dog {
            name
            barks
          }
          ... on Cat {
            name
            meows
          }
        }

        fragment FriendFields on Named {
          __typename
          name
          ... on Dog {
            barks
          }
          ... on Cat {
            meows
          }
        }
      """,
      Map(
        "data" -> Map(
          "__typename" -> "Person",
          "name" -> "Bob",
          "pets" -> List(
            Map("__typename" -> "Cat", "name" -> "Garfield", "meows" -> false),
            Map("__typename" -> "Dog", "name" -> "Odie", "barks" -> true)
          ),
          "friends" -> List(
            Map("__typename" -> "Person", "name" -> "Liz"),
            Map("__typename" -> "Dog", "name" -> "Odie", "barks" -> true)
          )
        )
      )
    )
  }
}