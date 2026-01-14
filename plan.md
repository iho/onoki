# Onoki Android Support - Implementation Plan

## Goal
Run Onoki applications natively on Android with UI capabilities.

## Architecture

```
┌──────────────┐    ┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│    .onoki    │───►│   .class     │───►│    .dex      │───►│     .apk     │
│   (source)   │    │  (bytecode)  │    │  (dalvik)    │    │  (android)   │
└──────────────┘    └──────────────┘    └──────────────┘    └──────────────┘
       │                   │                   │                   │
    onokic              jvmgen.ml            d8 tool           bundletool
```

---

## Phase 1: DEX Integration (1-2 days)

Add `d8` compiler integration to convert JVM bytecode to DEX.

#### [MODIFY] [bin/main.ml](file:///Users/ihor/onoki/bin/main.ml)
- Add `--android` flag to compile for Android
- After generating `.class` files, invoke `d8` to produce `classes.dex`

#### [NEW] [bin/android.ml](file:///Users/ihor/onoki/bin/android.ml)
- `run_d8`: Shell out to d8 tool
- `find_android_sdk`: Locate Android SDK on system
- `create_dex`: Convert class files to DEX

**Example usage:**
```bash
onoki --android app.onoki
# Outputs: classes.dex
```

---

## Phase 2: Android Project Template (2-3 days)

Create a template Android project that loads Onoki code.

#### [NEW] android/template/
```
android/template/
├── app/
│   ├── src/main/
│   │   ├── AndroidManifest.xml
│   │   ├── java/onoki/android/
│   │   │   ├── MainActivity.java      # Host activity
│   │   │   └── OnokiRuntime.java      # Bridges Onoki ↔ Android
│   │   └── res/
│   │       ├── layout/activity_main.xml
│   │       └── values/strings.xml
│   └── build.gradle
├── build.gradle
└── settings.gradle
```

#### MainActivity.java
```java
public class MainActivity extends AppCompatActivity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        OnokiRuntime.init(this);
        OnokiRuntime.callMain();  // Calls Main.main()
    }
}
```

---

## Phase 3: Android UI Bindings (1 week)

Add externals for common Android UI components.

#### [NEW] [stdlib/android.onoki](file:///Users/ihor/onoki/stdlib/android.onoki)
```ocaml
(* View operations *)
external find_view : int -> view = "virtual android/app/Activity findViewById (I)Landroid/view/View;"
external set_text : view -> string -> unit = "virtual android/widget/TextView setText (Ljava/lang/CharSequence;)V"
external get_text : view -> string = "virtual android/widget/TextView getText ()Ljava/lang/CharSequence;"

(* Button handling *)
external on_click : view -> (view -> unit) -> unit = "..."

(* Toast messages *)
external show_toast : string -> unit = "static onoki/android/OnokiRuntime showToast (Ljava/lang/String;)V"

(* Layout inflation *)
external set_content_view : int -> unit = "virtual android/app/Activity setContentView (I)V"
```

#### Required Compiler Changes
| File | Change |
|------|--------|
| `lexer.mll` | Add `view` as built-in type keyword |
| `ast.ml` | Add `TyView` to type definitions |
| `jvmgen.ml` | Handle `view` type in boxing/unboxing |

---

## Phase 4: Activity Lifecycle (3-4 days)

Support Android activity lifecycle callbacks.

#### [NEW] Lifecycle Annotations
```ocaml
(* Define lifecycle handlers *)
let on_create = fun () ->
  set_content_view R.layout.main
  let btn = find_view R.id.button1
  on_click btn (fun _ -> show_toast "Clicked!")

let on_resume = fun () ->
  (* Called when activity resumes *)
  ()

let on_pause = fun () ->
  (* Called when activity pauses *)
  ()
```

#### OnokiRuntime.java Changes
```java
public class OnokiRuntime {
    private static Activity activity;
    
    public static void init(Activity act) { activity = act; }
    
    public static void callOnCreate() {
        // Call Onoki's on_create function via reflection
        Main.class.getMethod("on_create").invoke(null);
    }
}
```

---

## Phase 5: Android Stdlib Extensions (1 week)

Add comprehensive Android API coverage.

#### Modules to Add
| Module | APIs |
|--------|------|
| `Android.View` | findViewById, setVisibility, setEnabled |
| `Android.Text` | setText, getText, addTextChangedListener |
| `Android.Button` | setOnClickListener, setEnabled |
| `Android.List` | RecyclerView adapter bindings |
| `Android.Intent` | startActivity, getIntent, putExtra |
| `Android.Storage` | SharedPreferences, file I/O |
| `Android.Network` | HTTP client, JSON parsing |
| `Android.Async` | runOnUiThread, coroutine support |

---

## Phase 6: Native DEX Generation (Optional, 2-3 weeks)

Generate DEX bytecode directly instead of via d8.

#### [NEW] [src/dexfile.ml](file:///Users/ihor/onoki/src/dexfile.ml)
- DEX file format structures
- String/type/method ID pools
- DEX file writer

#### [NEW] [src/dexinstr.ml](file:///Users/ihor/onoki/src/dexinstr.ml)
- Dalvik instruction set encoder
- Register allocation (Dalvik is register-based, not stack-based)

#### [NEW] [src/dexgen.ml](file:///Users/ihor/onoki/src/dexgen.ml)
- Main code generator for DEX
- Similar to `jvmgen.ml` but targeting Dalvik instructions

> [!WARNING]
> This phase requires significant effort. The d8 approach from Phase 1 is recommended for MVP.

---

## Build & Deploy Workflow

```bash
# 1. Create new Android project
onoki new-android MyApp

# 2. Write Onoki code
cat > app.onoki << 'EOF'
include "stdlib/android.onoki"

let on_create = fun () ->
  set_content_view 0x7f0a0000  (* R.layout.main *)
  let btn = find_view 0x7f080001  (* R.id.button *)
  on_click btn (fun _ -> show_toast "Hello from Onoki!")
EOF

# 3. Build APK
onoki build --android --release

# 4. Install and run
adb install -r app.apk
adb shell am start -n com.example.myapp/.MainActivity
```

---

## Timeline Summary

| Phase | Task | Duration | Priority |
|-------|------|----------|----------|
| 1 | DEX Integration (d8) | 1-2 days | 🔴 Critical |
| 2 | Android Project Template | 2-3 days | 🔴 Critical |
| 3 | Android UI Bindings | 1 week | 🟡 High |
| 4 | Activity Lifecycle | 3-4 days | 🟡 High |
| 5 | Android Stdlib | 1 week | 🟢 Medium |
| 6 | Native DEX Generation | 2-3 weeks | 🔵 Optional |

**Total MVP (Phases 1-4):** ~2-3 weeks
**Full Implementation:** ~5-6 weeks

---

## Next Steps

1. Install Android SDK and locate `d8` tool
2. Create `bin/android.ml` with d8 integration
3. Create minimal Android project template
4. Test with "Hello World" Onoki app on emulator
