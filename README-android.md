# PROVISIONAL — Learning to Build and Run Android Apps

This note records a possible route into Android development. It is provisional:
the tools, APIs, and exact project shape should be checked again when the work
begins.

## Motivation

The immediate prompt was using a Pixel 8 Pro as a small computer connected to a
Dell U2913WM through a DisplayPort dock. After restarting the phone and display
path, Android exposed a sharp 2559×1080 canvas. Termux works well enough for
now, but Android 17 still leaves system decoration at the top of the mirrored
display even with these Termux properties:

```properties
fullscreen = true
use-fullscreen-workaround = true
```

Rather than making a risky one-off replacement for the working Termux install,
the longer-term aim is to learn the normal Android build, signing, installation,
and debugging cycle.

## First project: an external-display probe

Start with a small app under its own package name. It should:

- enter Android's current immersive/edge-to-edge mode;
- display the active display ID, physical and application dimensions, density,
  refresh rate, window insets, and supported modes;
- say whether it is running on the built-in or external display;
- make status-bar, navigation-bar, cutout, and window-decoration insets visible;
- distinguish mirrored output from an activity hosted independently on an
  external display;
- provide simple controls for changing the requested system-bar behaviour.

This app is useful even if Termux is never modified: it gives us a known-small
program for understanding what the Pixel, dock, monitor, and Android window
manager are doing.

## Toolchain and working loop

1. Install the Android SDK command-line tools (Android Studio is optional),
   platform tools, the required SDK platform, and build tools.
2. Put `adb` on the development machine's path.
3. Enable Developer options and Wireless debugging on the Pixel, then pair it
   with this machine.
4. Create the diagnostic app with Gradle and a deliberately unique application
   ID so it can coexist with Termux.
5. Build a debug APK, install it with `adb install`, and inspect it with
   `adb logcat`, `dumpsys display`, and `dumpsys window`.
6. Exercise it on the phone alone, while mirroring, and—if Android permits it—
   as an activity launched directly on the external display.
7. Keep a durable personal signing key before distributing or depending on an
   app. Back it up securely; Android updates require the same application ID
   and signing identity.

The first milestone is deliberately modest: one source checkout, one command
that builds, one command that installs, and a visible report whose measurements
agree with Android's diagnostic commands.

## Later: a Termux build

Only after the small app establishes the toolchain and shows which fullscreen
API works on Android 17 should we consider building Termux. The likely work is
to test or adapt the upstream immersive-mode change, build the appropriate
Termux source variant, and verify it first on a disposable installation.

The current phone has the Google Play Termux build
(`googleplay.2026.06.21`, package `com.termux`). A locally signed APK cannot
update that package in place: Android requires matching signatures. Moving to
our own Termux build therefore requires a planned migration:

1. record installed Termux and plug-in packages;
2. make and verify a complete backup of the Termux home and prefix;
3. preserve SSH access through another channel, since uninstalling Termux also
   removes the SSH server and its data;
4. uninstall Termux and any signature-coupled plug-ins;
5. install the locally signed build and matching plug-ins;
6. restore and verify the environment; and
7. disable Play Store updates for the locally maintained package.

Do not begin that migration merely to remove a cosmetic bar. First demonstrate
the desired system-bar behaviour in the external-display probe, and rehearse
backup and restoration with data that can be discarded.

## Questions to answer when resuming

- Does Android 17 still honour the older global `policy_control` immersive
  setting when issued through `adb`, or must the app use the current window
  insets controller APIs?
- Is the remaining top strip part of the mirrored phone status bar, an external
  display window header, or a cutout-safe inset?
- Can the Pixel launch the diagnostic activity directly on the Dell rather than
  mirror the built-in display?
- Which Termux tree corresponds to the installed Google Play variant, and does
  upstream's fullscreen fix apply cleanly to it?
- Which Termux data and plug-ins must be preserved before changing signing
  authorities?

## Current observed setup (2026-09-29)

- Phone: Google Pixel 8 Pro, Android 17.
- Monitor: Dell U2913WM through a dock's DisplayPort output.
- External display canvas after reboot: 2559×1080 at 60 Hz.
- Termux: Google Play build `googleplay.2026.06.21`.
- Development machine: Java 21 is installed; Android SDK and `adb` were not
  found during the initial inspection.
- Working remote access: Termux `sshd` reached through the `phone2` SSH tunnel;
  it must be restarted after the phone reboots.
