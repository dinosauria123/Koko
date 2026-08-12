# KOKO オリジナル(Winteracter GUI) vs Python GUI — コマンド/機能カバレッジ比較

生成日: 2026-08-12
比較元:
- オリジナル: /home/dino/KDP2/*.FOR の `INPUT=` / `WRITE(INPUT,*)` 発行箇所 + IDD_* ダイアログ 85個
- Python GUI: /home/dino/Koko/gui_py/*.py の `send_koko` 実装全件

## 判定基準
- **OK**: Python GUI のメニュー/ボタンから対応コマンドが send_koko 経由で実際に送信される
- **未実装**: メニュー項目はある（またはオリジナルに存在する）が send_koko を呼ばない、あるいはダイアログ自体が存在しない
- **部分**: 基本形のみ実装、派生形（RECT/ELIP/POLY/TILT 等）が未実装

---

## A. 解析グラフ（メニュー項目はあるが送信未実装）

| 機能 | オリジナル ダイアログ | オリジナル コマンド | Python GUI 状態 |
|------|----------------------|---------------------|-----------------|
| スポット図 | IDD_SPOT / SPOTENERGY / SPOTPLOT / SPOTWRITE | `SPOT RING` `SPOT RAND` `SPOT RECT` `SPD *` `SPDSAVE` | **実装済み** (SPOT RING+SPD+STATS FULL+OUT TP) — koko が gnuplot 出力、GUI 描画確認済み |
| 点列図 (DOTF) | IDD_DOTF | `DOTF` `PLTDOTF` `DIFLEICA NO,n` | **実装済み** (SPACE I/O+FAR+DOTF+PLTDOTF,,1) |
| 幾何 OTF (GOTF) | IDD_GOTF | `GOTF` `PLTGOTF` `GEOLEICA NO,n` | **実装済み** (SPACE I/O+FAR+GOTF+PLTGOTF,1) |
| PSF | IDD_PSF / PSFENERGY / PSFSTREAK / PSFSTREHL | `PSF` `PSFLOG` `PSFWRITE` `PSFROT` `STREAK *` `STREHL` `APSTREHL` | **実装済み** (PSFWRITE YES+PSFLOG+PSFPLOT YES+PSF,1+CAPFNOUT) |
| ファン図（横/縦/対角/OPD/CD/LA） | IDD_FAN1 | `FANS XFAN` `FANS YFAN` `FANS XYFAN` `FANS NFAN` `FANS PFAN` (+CD/LA/OPD) | **実装済み** (FANS <qualifier> — KDP2 RIMS サブルーチン経由。PTY/GUI で PNG 生成確認済み) |
| 単一光線追跡 | IDD_RAY | `RAY` `FOB` `AIMRAY ON` `PRXYZ ALL` `DRAWFAN` | **実装済み** (RayDialog: [Trace]→FOB+RAY+PRXYZ ALL（テキスト）、[Plot Fan]→FANS XFAN（図）。GUI 動作確認済み。koko の RAY 単体は追跡結果をテキスト出力しないため PRXYZ ALL で主光線座標を表示) |
| 近軸 (PARAXIAL) | IDD_PARAX1/2/3 | `FCHY ALL` `FCHX ALL` `PCD3 ALL` `SCD3 ALL` `PRXYZ ALL` `PRR ALL` | **実装済み** (menuParaxial 配下の各項目 → slot_text で対応コマンド送信。koko がテキスト出力。GUI 動作確認済み) |
| 表示制御 (VIE) | IDD_VIE | `VIE` `PLOT *`（NOTE/PEN/FRAME/AXIS/UPLOT） | 部分（VIE XZ のみ、PLOT 詳細制御なし） |

### A 群 実装メモ（2026-08-12 コミット c88725c）
- SPOT/DOTF/GOTF/PSF は「コマンド列修正のみ」で動作。koko が $HOME/gnuplot/drawcmd.gpl を書き、既存の slot_plot → gnuplot → PNG 描画基盤が its まま機能（KokoMainWindow 経由で drawcmd.gpl 更新を確認済み）。
- FAN 図は koko が事前の光線追跡データファイル（RF）を要求し、コマンドラインから PLTXFAN 単独では "NO ACTION TAKEN"。オリジナルは IDD_FAN1 ダイアログ内で光線セットを構築してから PLTXFAN を呼ぶ。専用ダイアログ作成が必要。
- RAY/PARAXIAL はメニュー項目自体が Python GUI に存在しない。新規ダイアログ作成が必要。

## B. 完全に欠落している編集/設定機能

| 機能 | オリジナル ダイアログ | オリジナル コマンド | Python GUI 状態 |
|------|----------------------|---------------------|-----------------|
| アポディゼーション | IDD_APOD | `APOD GAUSS` `APOD NONE` | **実装済み** (ApodDialog → APOD GAUSS,<val> / APOD NONE。GUI 動作確認済み) |
| 回折設定 | IDD_DIFSET | `DIFFOB` `DIFRAY` `DIFLEICA` | OK（実装済み） |
| 絞り面 (STOP) | IDD_STOPSURF | `ASTOP` `ASTOP EN` `ASTOP EX` `ASTOP ENEX` | 部分（New で REFS 送信のみ、ASTOP ダイアログなし） |
| 参照面 (REF) | IDD_REFSSURF | `REFS` | 部分（New で送信のみ） |
| 偏心 (DEC) | IDD_DEC | `DEC` `DEC 0 0 0` | 部分（New で DEC 0 0 0 送信のみ） |
| 開口/遮蔽（CLAP/COBS 派生形） | IDD_APECIRC/APERECT/APEELIP/APERCTK + IDD_APECIRC2/APERECT2/APEELIP2 | `CLAP`(円) `CLAP RECT` `CLAP ELIP` `CLAP RCTK` `CLAP TILT` `COBS`(円) `COBS RECT` `COBS ELIP` `COBS TILT` | **実装済み**（ApertureDialog=CLAP全形状、ObscurationDialog=COBS円/矩形/楕円。koko 受付確認済み。COBS RCTK(枠)は未実装） |
| 傾斜 (TILT 系列) | IDD_TILTS / IDD_TILT / IDD_TILTAUTO / IDD_TILTBEN / IDD_TILTRET / IDD_TILTDAR / IDD_TILTREV | `TILT`(基本3軸) `TILT AUTO` `TILT DARD` `TILT BEND` `TILT REV` `RTILT` `TILTD` | **実装済み**（TiltDialog: タイプ選択＋基本はα/β/γ入力。U L+CHG+対応TILTコマンド+EOS+RTG ALL。koko 受付確認済み） |
| パラメータ拾い (PIKUP) | IDD_PIKSLV / IDD_PIKED1/2/3 | `PIKUP *` (CV/RD/CC/TH/AD/AE/AF/AG/TOR/ALPHA/BETA/GAMMA/XD/YD/GLASS/PRO/PIVX/Y/Z 等44種) `PIKD *` `SLV` | **実装済み** (PikupDialog: 面番号+種別+値 → U L + PIKUP <TYPE>,<surf>,<val> + EOS + RTG ALL。koko が受付確認済み。PIKD/SLV は未実装) |
| ピボット軸 (PIVAXIS) | IDD_PIVAX | `PIVAXIS` `PIVOT` `PIVAXIS NORMAL/VERTEX` | 未実装 |
| ガラスライブラリ | IDD_GLASSP / EDGLASS / LLIB | `LIB GET/PUT/DEL` `LENADD` `LIBSAVE` `LIBREST` `GLASSP` | 部分（FINDGLASS のみ） |
| マクロ | IDD_MACRO | `MACSAVE` `MACREST` `MAC_EDIT` `MACROOPT` | 未実装 |
| 有効径 (APERTURE/CLAP) | IDD_APECIRC 系 | `CLAP` `COBS` `APCX` `APCY` `APX` `APY` `CAPFN` `CAPFNOUT` | **実装済み**（円形 CLAP: ApertureDialog → U L + CHG <surf> + CLAP <rad> <xdecenter> <ydecenter> 0 0 + EOS + RTG ALL。koko 受付確認済み。APCX/APCY/APX/APY/CAPFN は未実装） |
| 面種別 (SURTYPE) | — | `SURTYPE` | 未実装 |
| コーティング (COATING) | — | `COATING` | 未実装 |
| 多構成 (CONFIGS) | IDD_CONFIGS | `CONFIGS ALL` `UPDATE LENS` `CFG` | **実装済み** (menuLensData → CONFIGS ALL 配線済み。GUI 動作確認済み) |
| 非シーケンシャル (NSS) | — | `NSSLENO` `GLASSWV` `GET *VERT` `GRAOUT` | 未実装 |
| 公差 (TOLERANCING) | menuTolerancing | `TEL YES` 等 | 未実装 |

## C. 実装済み（OK）

| 機能 | コマンド |
|------|---------|
| レンズ新規/保存/読込 | `LENS` `LI` `LENSSAVE` `LENSREST` `RTG ALL` |
| Zemax/CODE-V 入出力 | `ZMX2PRG` `CV2PRG` `LENO ZMX` `LENO CV` `OUT FILE` |
| 面編集（挿入/削除/ガラス/曲率/厚み） | `U L` `CHG` `INS` `DEL` `RD` `CV` `TH` `GLASS` `FINDGLASS` `EOS` |
| 最適化 | `MERIT` `FLCLTH` `VARIABLES` `VB` `OPRD` `ITER` `ITER FULL` `IT P` `PFIND` `ROBB` |
| 基本表示 | `VIE` `VIE XZ` `PLOT` |
| 焦点/入射角 | `PY` `SCY FANG` |
| アポダイゼーション設定 | `APOD`（actionApod 経由、部分的） |
| 回折設定 | `DIFFOB` `DIFRAY` `DIFLEICA`（actionDifset 経由、OK） |

---

## 優先実装候補（ユーザー確認待ち）
1. 解析グラフ: SPOT / DOTF / GOTF / PSF / FAN / RAY / PARAXIAL — 最も目に見える不足
2. 高度編集: PIKUP / PIVAXIS / APERTURE / COATING / CONFIGS
3. ユーティリティ: マクロ / ガラスライブラリ / NSS / 公差

※ 各機能の正確なコマンド書式は /home/dino/KDP2 の対応 FOR（GUICODE.FOR, OPTIMIZE.FOR, SPOTGUI.FOR, DOTFGUI.FOR, GOTFGUI.FOR, PSFGUI.FOR, VARIABLES.FOR 等）を参照。
