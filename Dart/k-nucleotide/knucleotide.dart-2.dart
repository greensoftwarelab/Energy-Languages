/**
 * The Computer Language Benchmarks Game
 *
 * http://benchmarksgame.alioth.debian.org/
 *
 * Contributed by Dwayne Slater
 * Based on a Java implementation by James McIlree and Tagir Valeev
 */

library knucleotide;

import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

const codes = const [-1, 0, -1, 1, 3, -1, -1, 2];
const nucleotides = 'ACGT';

/**
 * Contains a map from keys to occurrence count.
 */
class Result {
  final Map<int, int> map = {};
  Result();

  @override
  String toString() => "$map";
}

/**
 * A pair of keys to counts.
 */
class KeyPair {
  final String key;
  final int count;
  KeyPair(this.key, this.count);
}

/**
 * Converts a slice of the sequence into a key.
 */
int getKey(List<int> arr, int offset, int length) {
  int key = 0;
  int lastOffset = offset + length;
  for (int i = offset; i < lastOffset; i++) {
    key = (key << 2) + arr[i];
  }
  return key;
}

/**
 * Creates a map of all the times any key occurs in the sequence.
 */
Result createFragmentMap(List<int> sequence, int offset, int fragmentLength) {
  var res = new Result();
  int lastIndex = sequence.length-fragmentLength+1;
  for (int index = offset; index < lastIndex; index += fragmentLength) {
    var key = getKey(sequence, index, fragmentLength);
    res.map[key] = (res.map[key] ?? 0)+1;
  }
  return res;
}

/**
 * Combines two [Result]s together.
 */
Result sumTwoMaps(Result map1, Result map2) {
  map2.map.forEach((key, value) => map1.map[key] += value);
  return map1;
}

/**
 * Converts a key to a string.
 */
String keyToString(int key, int length) {
  var res = new List<String>(length);
  for (int i = 0; i < length; i++) {
    res[length - i - 1] = nucleotides[key & 0x3];
    key >>= 2;
  }
  return res.join();
}

/**
 * Write out the frequencies for each key that appears in the given [Result].
 */
String writeFrequencies(double totalCount, int keyLength, Result frequencies) {
  var freq = new List<KeyPair>(frequencies.map.length);
  var i = 0;
  frequencies.map.forEach((key, cnt) => freq[i++] = new KeyPair(keyToString(key, keyLength), cnt));
  freq.sort((a, b) => b.count.compareTo(a.count));
  var sb = new StringBuffer();
  freq.forEach((entry) {
    sb.write("${entry.key} ${(entry.count * 100.0/totalCount).toStringAsFixed(3)}\n");
  });
  return (sb..write("\n")).toString();
}

/**
 * Converts a character list into a list of codes.
 */
List<int> toCodes(List<int> sequence) {
  var l = sequence.length;
  var result = new List<int>(l);
  for (int i = 0; i < l; i++) {
    result[i] = codes[sequence[i] & 0x7];
  }
  return result;
}

/**
 * Write out the occurrences of the given nucleotideFragment in the list of
 * pending results.
 */
Future<String> writeCount(List<Future<Result>> futureResults, String nucleotideFragment) async {
  var key = toCodes(nucleotideFragment.codeUnits);
  var k = getKey(key, 0, key.length);
  var count = 0;
  await Future.wait(futureResults.map((future) {
    return future.then((f) {
      count += f.map[k] ?? 0;
    });
  }).toList());
  return "$count\t$nucleotideFragment\n";
}

/**
 * Reads sequence data from stdin.
 */
FutureOr<List<int>> read({bool sync: false}) async {
  if (!sync) {
    // The old Dart k-nucleotide benchmark used stdin.readLineSync, which is
    // way slower than doing async stream transforms.
    var three = false;
    var builder = new BytesBuilder(copy: false);
    StreamSubscription<List<int>> sub;
    sub = stdin
      .transform(const Latin1Decoder())
      .transform(const LineSplitter())
      .listen((line) {
        if (three) {
          if (line[0] != '>') {
            builder.add(line.codeUnits);
          } else {
            sub.cancel();
          }
        } else if (line.startsWith('>THREE')) {
          three = true;
        }
      });
    await sub.asFuture();
    var bytes = builder.takeBytes();
    return toCodes(bytes);
  } else {
    var encoding = Encoding.getByName("ISO_8859-1:1987");
    String line;
    while (stdin.readLineSync().substring(0, 6) != '>THREE');

    var builder = new BytesBuilder(copy: false);
    while ((line = stdin.readLineSync(encoding: encoding)) != null && line[0] != '>') {
      builder.add(line.codeUnits);
    }

    var bytes = builder.takeBytes();
    return toCodes(bytes);
  }
}

/**
 * An object that runs on an Isolate.
 */
abstract class Task {
  run();
}

/**
 * The sequence data for the current Isolate.
 */
List<int> isolateSequence;

/**
 * Initializes the per-isolate sequence data.
 */
class InitIsolateSequenceTask extends Task {
  final String sequence;

  InitIsolateSequenceTask(this.sequence);

  void run() {
    isolateSequence = sequence.codeUnits;
  }
}

/**
 * Creates a FragmentMap using the sequence data and the given parameters.
 */
class CreateFragmentMapTask extends Task {
  final int index, fragmentLength;
  CreateFragmentMapTask(this.index, this.fragmentLength);

  run() => createFragmentMap(isolateSequence, index, fragmentLength);
}

/**
 * Handles the submission and completion of tasks sent to an Isolate.
 */
class IsolateHandler {
  final Isolate _isolate;
  final SendPort _taskport;
  final RawReceivePort _port;
  final Queue<Completer> _completers = new Queue<Completer>();
  IsolateHandler._(this._isolate, this._taskport, this._port);

  /**
   * Creates an IsolateHandler.
   */
  static Future<IsolateHandler> create() async {
    IsolateHandler handler;
    var completer = new Completer<SendPort>.sync();
    var port = new RawReceivePort((data) {
      if (!completer.isCompleted) {
        completer.complete(data);
      } else {
        handler._finish(data);
      }
    });
    var isolate = await Isolate.spawn(_runner, port.sendPort);
    var taskport = await completer.future;
    return handler = new IsolateHandler._(isolate, taskport, port);
  }

  /**
   * Main "loop" of the task running Isolate.
   */
  static void _runner(SendPort inport) {
    var port = new RawReceivePort((Task task) {
      inport.send(task.run());
    });
    inport.send(port.sendPort);
  }

  void _finish(data) {
    _completers.removeFirst().complete(data);
  }

  /**
   * Schedules a task to the underlying Isolate.
   */
  Future<T> schedule<T>(Task task) {
    var c = new Completer<T>.sync();
    _taskport.send(task);
    _completers.add(c);
    return c.future;
  }

  /**
   * Gets the current load on the Isolate.
   */
  int get weight => _completers.length;

  /**
   * Closes the IsolateHandler and cleans up associated resources.
   */
  void close() {
    _isolate.kill();
    _port.close();
  }
}

/**
 * Manages multiple [IsolateHandler] objects.
 * Allows tasks to be scheduled on the least loaded Isolate.
 */
class IsolateExecutor {
  final List<IsolateHandler> _handlers;

  IsolateExecutor._(this._handlers);

  /**
   * Create an IsolateExecutor. By default this creates an isolate for each
   * core of the current running system.
   */
  static Future<IsolateExecutor> create({int n}) async {
    n ??= Platform.numberOfProcessors;
    var handlers = await Future.wait(new Iterable.generate(n, (_) => IsolateHandler.create()));
    return new IsolateExecutor._(handlers);
  }

  /**
   * Schedules a task on ALL of the IsolateHandlers.
   */
  Future<List<T>> scheduleAll<T>(Task task) {
    var futures = _handlers.map((handler) => handler.schedule(task)).toList();
    return Future.wait(futures);
  }

  /**
   * Schedules a task on the least loaded IsolateHandler.
   */
  Future<T> schedule<T>(Task task) {
    var leastLoad = _handlers.first;
    var leastLoadScore = leastLoad.weight;
    for (int i=1; i<_handlers.length; i++) {
      var handler = _handlers[i];
      var score = handler.weight;
      if (score < leastLoadScore) {
        leastLoad = handler;
      }
    }
    return leastLoad.schedule(task);
  }

  /**
   * Closes all IsolateHandlers.
   */
  void close() {
    _handlers.forEach((h) => h.close());
  }
}

/**
 * Dispatches fragment map creation to an isolate,
 */
List<Future<Result>> dispatchFragmentTasks(IsolateExecutor executor, int fragmentLength) {
  List<Future<Result>> tasks = [];
  for (int index = 0; index < fragmentLength; index++) {
    tasks.add(executor.schedule(new CreateFragmentMapTask(index, fragmentLength)));
  }
  return tasks;
}

main() async {
  // Start creating the executor while we wait on IO to finish
  var executorFuture = IsolateExecutor.create();
  // Read the data from stdin
  var sequence = await read();
  // Wait on the executor to start if it hasn't finished yet
  var executor = await executorFuture;

  // We start by intializing all the executor isolates with the sequence we read
  // Unfortunately, Dart has to copy all the data when doing this
  // To optimize this a bit, we can send it as a String :P (Saves about 20 seconds from my testing)
  executor.scheduleAll(new InitIsolateSequenceTask(new String.fromCharCodes(sequence)));

  var futureBuffer = <Future<String>>[
    // Dispatch a task to get the frequencies with a fragment length of 1
    dispatchFragmentTasks(executor, 1)[0]
      .then((result) => writeFrequencies(sequence.length.toDouble(), 1, result)),
    // Dispatch a task to get the frequencies with a fragment length of 2
    Future.wait(dispatchFragmentTasks(executor, 2))
      .then((results) => writeFrequencies((sequence.length-1).toDouble(), 2, sumTwoMaps(results[0], results[1]))),
  ];
  // Dispatch tasks for each of the nucleotideFragments
  const ["GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"].forEach((nucleotideFragment) {
    futureBuffer.add(writeCount(dispatchFragmentTasks(executor, nucleotideFragment.length), nucleotideFragment));
  });

  // Wait for everything to finish, then join the resulting output together
  stdout.write((await Future.wait(futureBuffer)).join());
  executor.close();
}