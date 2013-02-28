package test

import org.specs2.mutable._

import models.LanguageAST._
import models.Parser
import models.LanguageAST.ExpressionImplicits._

import Utils._

class ParserSpec extends Specification {
  "Parser" should {

    "parse a simple numerical expression such as \"32\"" in {
      val expression = "32"
      Parser.parseSingleValidStatement(expression) must be equalTo ObjectExpression(Number_(32))
    }
    
    "parse a simple addition expression such as \"7.+(4)\"" in {
      val expression = "7.+(4)"
      val actual = Parser.parseSingleValidStatement(expression)
      actual must be equalTo simpleMethodCall(7.0, 4.0, "+")
    }
    
    "parse a simple addition expression with references such as \"b.+(c)\"" in {
      val expression = "b.+(c)"
      val actual = Parser.parseSingleValidStatement(expression)
      actual must be equalTo simpleMethodCall("b", "c", "+")
    }
    
    "parse a simple substraction expression with references such as \"b.-(c)\"" in {
      val expression = "b.-(c)"
      val actual = Parser.parseSingleValidStatement(expression)
      actual must be equalTo simpleMethodCall("b", "c", "-")
    }
    
    "parse an infix operator notation addition expression such as \"7 + 4\"" in {
      val expression = "7 + 4"
      val actual = Parser.parseSingleValidStatement(expression)
      actual must be equalTo simpleMethodCall(7, 4, "+")
    }
    
    "parse a simple assignement" in {
      val assignement = "a := 7"
      val actual = Parser.parseSingleValidStatement(assignement)
      actual must be equalTo Assignement(Reference("a"), ObjectExpression(Number_(7)))
    }
     
    "parse multiple statements" in {
      val code = """a := b.+(c)
b := 10
c := 5
a"""
      val actual = Parser.parse(code)
      val statement1 = Assignement(Reference("a"), simpleMethodCall("b", "c", "+"))
      val statement2 = Assignement(Reference("b"), ObjectExpression(Number_(10)))
      val statement3 = Assignement(Reference("c"), ObjectExpression(Number_(5)))
      val statement4 = Reference("a", None)
      val expected = Block(List(statement1, statement2, statement3, statement4))
      actual must be equalTo expected
    }
    
    "parse the simple \"true\" expression" in {
      val code = "true"
      
      val actual = Parser.parseSingleValidStatement(code)
      val expected = ObjectExpression(Boolean_(true))
      actual must be equalTo expected
    }
    
    "parse the simple \"false\" expression" in {
      val code = "false"
      
      val actual = Parser.parseSingleValidStatement(code)
      val expected = ObjectExpression(Boolean_(false))
      actual must be equalTo expected
    }
    
    "parse a comparison between true and false" in {
      val code = "true.==(false)"
      
      val actual = Parser.parseSingleValidStatement(code)
      actual must be equalTo simpleMethodCall(true, false, "==")
    }
    
    "parse a simple pass if expression" in {
      val code = "if(true) 13"
  
      val actual = Parser.parseSingleValidStatement(code)
      val expected = IfExpression(ObjectExpression(Boolean_(true)), Block(ObjectExpression(Number_(13))))
      actual must be equalTo expected
    }
    
    "parse a simple fail if expression" in {
      val code = "if(false) 2"
      
      val actual = Parser.parseSingleValidStatement(code)
      val expected = IfExpression(ObjectExpression(Boolean_(false)), Block(ObjectExpression(Number_(2))))
      actual must be equalTo expected
    }
    
    "parse a simple pass if/else expression" in {
      val code = "if(true) 2 else 10"
      
      val actual = Parser.parseSingleValidStatement(code)
      val expected = IfExpression(ObjectExpression(Boolean_(true)),
				  Block(ObjectExpression(Number_(2))),
				  Some(Block(ObjectExpression(Number_(10)))))
      actual must be equalTo expected
    }
    
    "parse a simple fail if/else expression" in {
      val code = "if(false) 3 else 9"
      
      val actual = Parser.parseSingleValidStatement(code)
      val expected = IfExpression(ObjectExpression(Boolean_(false)),
				  Block(ObjectExpression(Number_(3))),
				  Some(Block(ObjectExpression(Number_(9)))))
      actual must be equalTo expected
    }
    
    "parse a complexe if expression" in {
      val code = "if(a.==(b)) 3.+(7)"
      
      val actual = Parser.parseSingleValidStatement(code)
      val expected = IfExpression(simpleMethodCall("a", "b", "=="), Block(simpleMethodCall(3, 7, "+")))
      actual must be equalTo expected
    }
    
    "parse a complexe if/else expression" in {
      val code = "if(ggg.<~>(hj)) a else b"
      
      val actual = Parser.parseSingleValidStatement(code)
      val expected = IfExpression(simpleMethodCall("ggg", "hj", "<~>"),
				  Block(Reference("a")),
				  Some(Block(Reference("b"))))
      actual must be equalTo expected
    }
    
    "parse a simple pass multiline if expression" in {
      val code = """if(true):
	4"""

      val actual = Parser.parse(code)
      val expected = Block(IfExpression(ObjectExpression(Boolean_(true)), Block(ObjectExpression(Number_(4)))))
      actual must be equalTo expected
    }
    
    "parse a simple fail multiline if expression" in {
      val code = """if(false):
	4"""

      val actual = Parser.parse(code)
      val expected = Block(IfExpression(ObjectExpression(Boolean_(false)), Block(ObjectExpression(Number_(4)))))
      actual must be equalTo expected
    }
    
    "parse a complexe multiline if expression" in {
      val code = """if(a.==(7.+(9))):
	a.+(g)"""
  
      val actual = Parser.parse(code)
      val expected = Block(IfExpression(simpleMethodCall(Reference("a"), simpleMethodCall(7, 9, "+"), "=="),
                                        Block(simpleMethodCall("a", "g", "+"))))
      actual must be equalTo expected
    }
    
	"parse a simple pass multiline if/else expression" in {
		val code = """if(true):
	7
else:
	3"""
		val actual = Parser.parse(code)
		val expected = Block(IfExpression(ObjectExpression(Boolean_(true)), Block(ObjectExpression(Number_(7))), Some(Block(ObjectExpression(Number_(3))))))
		actual must be equalTo expected
	}
	
	"parse a simple fail multiline if/else expression" in {
		val code = """if(false):
	7
else:
	3"""
		val actual = Parser.parse(code)
		val expected = Block(IfExpression(ObjectExpression(Boolean_(false)), Block(ObjectExpression(Number_(7))), Some(Block(ObjectExpression(Number_(3))))))
		actual must be equalTo expected
	}
	
	"parse a complexe multiline if/else expression" in {
		val code = """if(a.==(b)):
	a.+(b)
else:
	a.-(b)"""
		val actual = Parser.parse(code)
		val expected = Block(IfExpression(simpleMethodCall("a", "b", "=="), Block(simpleMethodCall("a", "b", "+")), Some(Block(simpleMethodCall("a", "b", "-")))))
		actual must be equalTo expected
	}
	
	"parse a simple function" in {
		val code = "() => 5"
		
		val actual = Parser.parseSingleValidStatement(code)
		val expected = ObjectExpression(new Function(Block(ObjectExpression(Number_(5)))))
		actual must be equalTo expected
	}
	
	"parse a simple parameterized function" in {
		val code = "(a) => 6"
		
		val actual = Parser.parseSingleValidStatement(code)
		val expected = ObjectExpression(new Function(Block(ObjectExpression(Number_(6))), List(Param("a"))))
		actual must be equalTo expected
	}
	
	"parse a simple multiparameterized function" in {
		val code = "(a, b) => 6"
		
		val actual = Parser.parseSingleValidStatement(code)
		val expected = ObjectExpression(new Function(Block(ObjectExpression(Number_(6))), List(Param("a"), Param("b"))))
		actual must be equalTo expected
	}
	
	"parse a complexe function" in {
		val code = "() => 5.+(9)"
		
		val actual = Parser.parseSingleValidStatement(code)
		val expected = ObjectExpression(new Function(Block(simpleMethodCall(5, 9, "+"))))
		actual must be equalTo expected
	}
	
	"parse a complexe parameterized function" in {
		val code = "(car) => car.isModel(model)"
		
		val actual = Parser.parseSingleValidStatement(code)
		val expected = ObjectExpression(new Function(Block(simpleMethodCall("car", "model", "isModel")), List(Param("car"))))
		actual must be equalTo expected
	}
	
	"parse a complexe multiparameterized function" in {
		val code = "(car, bogus) => car.isModel(model)"
		
		val actual = Parser.parseSingleValidStatement(code)
		val expected = ObjectExpression(new Function(Block(simpleMethodCall("car", "model", "isModel")), List(Param("car"), Param("bogus"))))
		actual must be equalTo expected
	}
	
	"parse a simple function assignement" in {
		val code = "a := () => 5"
		
		val actual = Parser.parseSingleValidStatement(code)
		val expected = Assignement(Reference("a"), new Function(Block(ObjectExpression(Number_(5)))))
		actual must be equalTo expected
	}
	
	"parse a simple function assignement 2" in {
		val code = "bbe := () => 7"
		
		val actual = Parser.parseSingleValidStatement(code)
		val expected = Assignement(Reference("bbe"), new Function(Block(ObjectExpression(Number_(7)))))
		actual must be equalTo expected
	}
	
	"parse a simple function application" in {
		val code = "a()"
		
		val actual = Parser.parseSingleValidStatement(code)
		val expected = Application(Reference("a"))
		actual must be equalTo expected
	}
	
	val bug_1 = "michel_function(0)"
	"bug1" in {
		val expected = Application(Reference("michel_function"), List(ObjectExpression(Number_(0))))
		val actual = Parser.parseSingleValidStatement(bug_1)
		actual must be equalTo expected
	}
  }
}