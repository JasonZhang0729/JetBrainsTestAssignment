import play.api.libs.{json => playJson}

object JSONSerializationTesting {
  def main(args: Array[String]): Unit = {
    val serialization = new Serialize();
    val deserialization = new Deserialize();

    println("++++++++++++++++(1)++++++++++++++++");
    var t1 = null;
    println(serialization.serialize(t1));
    println("Expected: " + playJson.JsString(""));
    println("");
    println(deserialization.deserialize(t1));
    println("Expected: " + null);

    println("++++++++++++++++(2)++++++++++++++++");
    var t2 = Or(And(And(True, True),False), Not(True));
    println(serialization.serialize(t2));
    println("Expected: " + "Or(And(And(True,True),False),Not(True))");
    println("");
    println(deserialization.deserialize(serialization.serialize(t2)) == t2);
    println("Expected: " + true);

    println("++++++++++++++++(3)++++++++++++++++");
    var t3 = And(Variable("A"), Not(Or(Variable("B"), Variable("C"))));
    println(serialization.serialize(t3));
    println("Expected: " + "And(A,Not(Or(B,C)))");
    println("");
    println(deserialization.deserialize(serialization.serialize(t3)) == t3);
    println("Expected: " + true);

    println("++++++++++++++++(4)++++++++++++++++");
    var t4 = Or(And(True, Not(Variable("Apple"))), And(Variable("Pear"), Or(True, Not(False))));
    println(serialization.serialize(t4));
    println("Expected: " + "Or(And(True,Not(Variable(Apple))),And(Variable(Pear),Or(True,Not(False))))");
    println();
    println(deserialization.deserialize(serialization.serialize(t4)) == t4)
    println("Expected: " + true);
  }
}
