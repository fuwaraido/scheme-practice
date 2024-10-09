
import re

def flatten(a):
    
    if a == ():
        return ()
    elif isinstance(a, tuple):
        if isinstance(a[0], tuple):
            return (flatten(a[0]),) + flatten(a[1])
        else:
            return (a[0],) + flatten(a[1])
    else:
        return (a,)


def process(processor): # create scope
    def decorator(method):
        def wrapper(self, *args, **kwargs):
            res = method(self, *args, **kwargs)
            match res:
                case Ok(): return Ok(res._input, processor(res.value))
                case _: return res
        return wrapper
    return decorator


processor = flatten


class Parser:

    def __init__(self):
        self.astcon = lambda x: x
        
    def parse(self, _input):
        res = self._parse(_input)
        match res:
            case Ok(): return Ok(res._input, self.astcon(res.value))
            case _: return res
   
    def _parse(self, _input):
        raise NotImplementedError

    def __mul__(self, other):
        if isinstance(other, str):
            return AndParser(self, Token(other))
        else:
            return AndParser(self, other)

    def __or__(self, other):
        if isinstance(other, str):
            return OrParser(self, Token(other))
        else:
            return OrParser(self, other)

    def _not(self):
        return Not(self)

    def _try(self):
        return Try(self)

    def _opt(self):
        return Opt(self)

    def _nofail(self):
        return Nofail(self)

    def _many(self):
        return Many(self)

    def _some(self):
        return Some(self)

    def set_constructor(self, astcon):
        self.astcon = astcon

       
class ParseResult:
    
    def __init__(self, _input):
        self._input = _input

    def __str__(self):
        return f"{type(self).__name__}(\"{self._input[:20]}...\":{len(self._input)})"


#
# failed to match, without consuming any token
#
class Fail(ParseResult):
    
    def __init__(self, _input):
        super().__init__(_input)
        self.value = ()


#
# succeeded to match, without consuming any token
#
class Epsn(ParseResult):
    
    def __init__(self, _input):
        super().__init__(_input.lstrip())
        self.value = ()

#
# succeeded to match, after consumed at least one token
#
class Ok(ParseResult):
    
    def __init__(self, _input, value):
        super().__init__(_input)
        self.value = value

    def __str__(self):
        return f"Ok(\"{self._input[:20]}...\":{len(self._input)} ,{str(self.value)})"

#
# failed to match, after consumed at least one token
#
class Error(ParseResult):
    
    def __init__(self, _input):
        super().__init__(_input)


class OrParser(Parser):
    
    def __init__(self, a, b):
        super().__init__()
        self.a = a
        self.b = b
    
    def _parse(self, _input):
        ra = self.a.parse(_input)
        match ra:
            case Ok(): return ra
            case Error(): return ra
            case Fail(): return self.b.parse(ra._input)
            case Epsn(): # Epsn
                rb = self.b.parse(ra._input)
                match rb:
                    case Ok(): return rb
                    case Fail(): return Epsn(_input)
                    case Error(): return rb
                    case Epsn(): return rb


class AndParser(Parser):
    
    def __init__(self, a, b):
        super().__init__()
        self.a = a
        self.b = b

    @process(processor) 
    def _parse(self, _input):
        ra = self.a.parse(_input)
        match ra:
            case Error(): return ra
            case Fail(): return ra
            case Epsn(): return self.b.parse(_input)
            case Ok(): # Ok                               
                rb = self.b.parse(ra._input)                
                match rb:
                    case Error(): return rb
                    case Fail(): return Error(rb._input)
                    case Epsn(): return Ok(rb._input, (ra.value,))
                    case Ok(): return Ok(rb._input, (ra.value, rb.value))


class Many(Parser):

    def __init__(self, p):
        super().__init__()
        self.p = p

    def _parse(self, _input):
        ra = self.p.parse(_input)
        match ra:
            case Ok():
                rb = self.parse(ra._input)
                return Ok(rb._input, (ra.value, rb.value))            
            case Fail():
                if _input == ra._input:
                    return Epsn(_input)
                else:
                    return Ok(ra._input, (ra.value,))
            case Epsn(): return ra
            case Error(): return rb


class Opt(Parser):

    def __init__(self, p):
        super().__init__()
        self.p = p

    def _parse(self, _input):
        res = self.p.parse(_input)
        match res:
            case Ok(): return res
            case Fail(): return Epsn(_input)
            case Error(): return res
            case Epsn(): return res


class Not(Parser):

    def __init__(self, p):
        super().__init__()
        self.p = p

    def _parse(self, _input):
        res = self.p.parse(_input)        
        match res:
            case Ok(): return Fail(_input)
            case _: return Epsn(_input)              

class Some(Parser):
    
    def __init__(self, p):
        super().__init__()
        self.p = p

    def _parse(self, _input):
        ra = self.p.parse(_input)
        match ra:
            case Ok():
                rb = self.parse(ra._input)
                return Ok(rb._input, (ra.value, rb.value))
            case Fail():
                if _input == ra._input:
                    return Fail(_input)
                else:
                    return Ok(ra._input, (ra.value,))
            case Epsn(): return Fail(_input)
            case Error(): return rb
            
class Try(Parser):

    def __init__(self, p):
        super().__init__()
        self.p = p

    def _parse(self, _input):
        res = self.p.parse(_input)
        match res:
            case Error(): return Fail(res._input)
            case _: return res

class Nofail(Parser):

    def __init__(self, p):
        super().__init__()
        self.p = p

    def _parse(self, _input):
        res = self.p.parse(_input)
        match res:
            case Fail(): return Error(res._input)
            case _: return res

class Regex(Parser):
    
    def __init__(self, regex):
        super().__init__()
        self.regex = regex

    def _parse(self, _input):
        matched = re.match(self.regex, _input)
        if matched:
            return Ok(_input[matched.end():], matched.group().strip())
        else:
            return Fail(_input)

class Token(Regex):

    def __init__(self, token):
        super().__init__(rf'{re.escape(token)}\s+')

class Pattern(Regex):

    def __init__(self, pattern):
        super().__init__(rf'{pattern}\s+')


class World:
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def __str__(self):
        return "\"" + self.a + "," + self.b + "\""

hello=Token("hello")
fuckin=Token("fuckin")
world=Token("world")
#helloworld=hello*fuckin._opt()*world
#helloworld=hello*fuckin._not()*world
#helloworld=hello._some()*world
helloworld = hello._many() * world | fuckin * world
helloworld.set_constructor(lambda x: World(x[0], x[1]))

#print(helloworld.parse("hello hello hello world "))
print(helloworld.parse("fuckin world "))











        
