``` ini

BenchmarkDotNet=v0.10.14, OS=macOS High Sierra 10.13.4 (17E202) [Darwin 17.5.0]
Intel Core i5-7360U CPU 2.30GHz (Kaby Lake), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.4
  [Host]     : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT DEBUG
  DefaultJob : .NET Core 2.0.5 (CoreCLR 4.6.0.0, CoreFX 4.6.26018.01), 64bit RyuJIT


```
|      Method |     Mean |     Error |    StdDev |
|------------ |---------:|----------:|----------:|
| renderScene | 475.6 ms | 0.6115 ms | 0.5720 ms |
