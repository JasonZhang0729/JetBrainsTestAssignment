import play.api.libs.{json => playJson}

sealed trait BooleanExpression
case object True extends BooleanExpression
case object False extends BooleanExpression
case class Variable(symbol: String) extends BooleanExpression
case class Not(e: BooleanExpression) extends BooleanExpression
case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression
case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression

class Serialize {
  /**
    * Serialize a expression represented by BooleanExpression case classes to JSON string
    * @param exp the expression represented by case classes that extend BooleanExpression
    * @return a JSON string that serializes the input expression
    */

  def serialize(exp: BooleanExpression): playJson.JsString = {
    if (exp == null) {
      return playJson.JsString("");
    }

    exp match {
      case True =>
        return playJson.JsString("True");

      case False =>
        return playJson.JsString("False");

      case Not(e) =>
        var p1 = serialize(e).value;
        var output = "Not(" + p1 + ")";
        return playJson.JsString(output);

      case Or(e1, e2) =>
        var p1 = serialize(e1).value;
        var p2 = serialize(e2).value;
        var output = "Or(" + p1 + "," + p2 + ")";
        return playJson.JsString(output);

      case And(e1, e2) =>
        var p1 = serialize(e1).value;
        var p2 = serialize(e2).value;
        var output = "And(" + p1 + "," + p2 + ")";
        return playJson.JsString(output);

      case Variable(symbol: String) =>
        return playJson.JsString(symbol);
    }
  }
}


class Deserialize {

  /**
    * Deserialize a JSON string to BooleanExpression case classes
    * @param jsonString a JSON String representing an object of type BooleanExpression
    * @return an object of type BooleanExpression
    */
  def deserialize(jsonString: playJson.JsString): BooleanExpression = {
    if(jsonString == null || jsonString.value.length == 0) {
      return null;
    }

    val notPattern = "(Not)[(](.*)[)]".r;
    val orPattern = "(Or)[(](.*)[)]".r;
    val andPattern = "(And)[(](.*)[)]".r;

    jsonString.value match {
      case "True" =>
        return True;

      case "False" =>
        return False;

      case notPattern("Not", str) =>
        var e = deserialize(playJson.JsString(str));
        return Not(e);

      case orPattern("Or", str)  =>
        //Find where the comma partition the string
        var count: Int = 0;
        var index: Int = 0;
        for (i <- 0 to (str.length() - 1)) {
          if (str.substring(i, i + 1).equals("(")) {
            count = count + 1;
          } else if (str.substring(i, i + 1).equals(")")) {
            count = count - 1;
          } else if (str.substring(i , i + 1).equals(",") && count == 0) {
            index = i;
          }
        }

        var e1 = deserialize(playJson.JsString(str.substring(0, index)));
        var e2 = deserialize(playJson.JsString(str.substring(index + 1)));

        return Or(e1, e2);

      case andPattern("And", str) =>
        var count: Int = 0;
        var index: Int = 0;
        for (i <- 0 to (str.length() - 1)) {
          if (str.substring(i, i + 1).equals("(")) {
            count = count + 1;
          } else if (str.substring(i, i + 1).equals(")")) {
            count = count - 1;
          } else if (str.substring(i , i + 1).equals(",") && count == 0) {
            index = i;
          }
        }

        var e1 = deserialize(playJson.JsString(str.substring(0, index)));
        var e2 = deserialize(playJson.JsString(str.substring(index + 1)));

        return And(e1, e2);

      case symbol: String =>
        return new Variable(symbol);
    }
  }
}
