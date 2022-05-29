# ZipCoder - Powerful zip-code validation

Powerful zip-code validation in a convenient CLI tool, written in Haskell.

Currently supporting NL and JP codes.

**Validating Dutch (NL) zip-codes**

Running:
```
λ cabal run zipcoder -- --country-code NL --zip-codes 1445KJ:1976HG
```

Output:
```
λ ZipCoder - Powerful zip-code validation
λ Starting ZipCoder after successful parsing of options..

λ Initiating validation for country NL..

λ All zip-codes are valid!
```

**Validating Japan (JP) zip-codes**

Running:
```
λ cabal run zipcoder -- --country-code JP --zip-codes 000-1234:000-34555
```

Output:
```
λ ZipCoder - Powerful zip-code validation
λ Starting ZipCoder after successful parsing of options..

λ Initiating validation for country JP..

[ERROR] Detected bad data! Bad entries will be printed below:

["000-34555"]
```