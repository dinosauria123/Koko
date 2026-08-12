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
| 表示制御 (VIE) | IDD_VIE | `VIE` `VIE XZ/XY/ORTHO` `VIEVIG ON/OFF` `VIESYM ON/OFF` `VIEOFF` `VIECO` `PLOT VIEW` `PLOT NOTE/PEN/FRAME/AXIS/UPLOT` | **実装済み**（VieDialog: 視点+XZ/XY/ORTHO+スケール+ビネット/対称トグル→VIEVIG/VIESYM+VIE <type>,<factor>。koko 受付確認済み。PLOT NOTE/PEN/FRAME/AXIS/UPLOT 詳細制御は未実装） |

### A 群 実装メモ（2026-08-12 コミット c88725c）
- SPOT/DOTF/GOTF/PSF は「コマンド列修正のみ」で動作。koko が $HOME/gnuplot/drawcmd.gpl を書き、既存の slot_plot → gnuplot → PNG 描画基盤が its まま機能（KokoMainWindow 経由で drawcmd.gpl 更新を確認済み）。
- FAN 図は koko が事前の光線追跡データファイル（RF）を要求し、コマンドラインから PLTXFAN 単独では "NO ACTION TAKEN"。オリジナルは IDD_FAN1 ダイアログ内で光線セットを構築してから PLTXFAN を呼ぶ。専用ダイアログ作成が必要。
- RAY/PARAXIAL はメニュー項目自体が Python GUI に存在しない。新規ダイアログ作成が必要。

## B. 完全に欠落している編集/設定機能

| 機能 | オリジナル ダイアログ | オリジナル コマンド | Python GUI 状態 |
|------|----------------------|---------------------|-----------------|
| アポディゼーション | IDD_APOD | `APOD GAUSS` `APOD NONE` | **実装済み** (ApodDialog → APOD GAUSS,<val> / APOD NONE。GUI 動作確認済み) |
| 回折設定 | IDD_DIFSET | `DIFFOB` `DIFRAY` `DIFLEICA` | OK（実装済み） |
| 絞り面 (STOP) | IDD_STOPSURF | `ASTOP` `ASTOP EN` `ASTOP EX` `ASTOP ENEX` | **実装済み**（StopDialog: 面番号+瞳孔調整オプション→U L+CHG+ASTOP[ EN/EX/ENEX]+EOS+RTG ALL。koko 受付確認済み） |
| 参照面 (REF) | IDD_REFSSURF | `REFS` | **実装済み**（RefDialog: 面番号+回転角→U L+CHG+REFS <rot>+EOS+RTG ALL。koko 受付確認済み） |
| 偏心 (DEC) | IDD_DEC | `DEC` `DEC 0 0 0` | **実装済み**（DecDialog: 面番号+X/Y/Z→U L+CHG+DEC X Y Z+EOS+RTG ALL。koko 受付確認済み。KDP2 の DEC,Y,X,Z 順序を X/Y/Z 表示にマップ） |
| 開口/遮蔽（CLAP/COBS 派生形） | IDD_APECIRC/APERECT/APEELIP/APERCTK + IDD_APECIRC2/APERECT2/APEELIP2 | `CLAP`(円) `CLAP RECT` `CLAP ELIP` `CLAP RCTK` `CLAP TILT` `COBS`(円) `COBS RECT` `COBS ELIP` `COBS TILT` | **実装済み**（ApertureDialog=CLAP全形状、ObscurationDialog=COBS円/矩形/楕円。koko 受付確認済み。COBS RCTK(枠)は未実装） |
| 傾斜 (TILT 系列) | IDD_TILTS / IDD_TILT / IDD_TILTAUTO / IDD_TILTBEN / IDD_TILTRET / IDD_TILTDAR / IDD_TILTREV | `TILT`(基本3軸) `TILT AUTO` `TILT DARD` `TILT BEND` `TILT REV` `RTILT` `TILTD` | **実装済み**（TiltDialog: タイプ選択＋基本はα/β/γ入力。U L+CHG+対応TILTコマンド+EOS+RTG ALL。koko 受付確認済み） |
| パラメータ拾い (PIKUP) | IDD_PIKSLV / IDD_PIKED1/2/3 | `PIKUP *` (CV/RD/CC/TH/AD/AE/AF/AG/TOR/ALPHA/BETA/GAMMA/XD/YD/GLASS/PRO/PIVX/Y/Z 等44種) `PIKD *` `SLV` | **実装済み** (PikupDialog: 面番号+種別+値 → U L + PIKUP <TYPE>,<surf>,<val> + EOS + RTG ALL。koko が受付確認済み。PIKD/SLV は未実装) |
| ピボット軸 (PIVAXIS) | IDD_PIVAX | `PIVAXIS NORMAL` `PIVAXIS VERTEX` `PIVOT,X,Y,Z` `PIVAXIS ?` | **実装済み**（PivaxisDialog: NORMAL または VERTEX(座標指定)、または表示のみ(PIVAXIS ?)→U L+CHG+PIVAXIS NORMAL / PIVAXIS VERTEX+PIVOT,X,Y,Z / PIVAXIS ?+EOS+RTG ALL。koko 受付確認済み） |
| ガラスライブラリ | IDD_GLASSP / EDGLASS / LLIB | `LIB GET/PUT/DEL` `LENADD` `LIBSAVE` `LIBREST` `GLASSP` | 部分（FINDGLASS + Lens Library LIB GET/PUT/DEL 実装。koko が LIB GET/PUT/DEL をサポートすることを確認済み。LIB REST/SAVE/LIST は koko 未サポート、GLASSP は送信のみ） |
| マクロ | IDD_MACRO | `MACSAVE` `MACREST` `MAC_EDIT` `MACROOPT` | 部分（MacroDialog: ライブラリ初期化 IMF+PROCEED（~/KODS/LIBMAC/ 未作成時のみ有効）、実行(MACRO name)/削除(MDEL name)/編集(MACED name→mac>モード)。koko が ~/KODS/LIBMAC/MAC.DAT を要求し、IMF+PROCEED で初期化できることを確認済み。MACED 編集時の複数行入力+MACSAVE は koko 側 mac> モードで手動扱い） |
| 有効径 (APERTURE/CLAP) | IDD_APECIRC 系 | `CLAP` `COBS` `APCX` `APCY` `APX` `APY` `CAPFN` `CAPFNOUT` | **実装済み**（円形 CLAP: ApertureDialog → U L + CHG <surf> + CLAP <rad> <xdecenter> <ydecenter> 0 0 + EOS + RTG ALL。koko 受付確認済み。APCX/APCY/APX/APY/CAPFN は未実装） |
| 面種別 (SURTYPE) | （KDP2 にも設定ダイアログなし / 表示コマンドのみ） | `SURTYPE <surface>` `SURTYPE ALL` | **実装済み**（SurtypeDialog: 面番号またはALL→SURTYPE <surf>/ALL 送信。koko が REAL/PARAXIAL をテキスト出力。GUI 経由で koko 受付確認済み。msgView 表示は既知の非同期読み取りタイミング問題あり＝PARAXIAL/RAY と同じ） |
| コーティング (COATING) | — | `COATING <n>` `COATING ?` | **実装済み**（CoatingDialog: 面番号+コーティング番号(0=none)、または表示のみ(COATING ?)→U L+CHG+COATING <n>/?+EOS+RTG ALL。koko 受付確認済み） |
| 多構成 (CONFIGS) | IDD_CONFIGS | `CONFIGS ALL` `UPDATE LENS` `CFG` | **実装済み** (menuLensData → CONFIGS ALL 配線済み。GUI 動作確認済み) |
| 非シーケンシャル (NSS) | — | `NSSNEW` `NSSUNITS` `NSSWV` `UNIVERSE` `OBJECT` `ONAME` `NSSSAVE` `NSSREST` `NSSTRACE` `NSSLIST` `NSSDEL` | **実装済み**（NssDialog: 新規作成/単位/波長/ユニバース/オブジェクト定義/レイトレース/一覧/保存/復元/削除ボタン。koko が NSSNEW でデータベース作成後、全 NSS コマンドをサポートすることを PTY で確認済み） |
| 公差 (TOLERANCING) | menuTolerancing | `TEL YES` 等 | **実装済み**（ToperDialog: TVARモードで公差変数(TH/RD_FR/CV_FR/CC/AD/AE/AF/AG/XD/YD/PIVX/Y/Z)定義→TOPERモードでオペランド(FUNCxx)定義→SENSI/MONTE解析。koko が TVAR→tvb>、TOPER→top>、SENSI/MONTE を完全サポートし、TVAR+TOPER+SENSI で感度解析レポートを出力することを PTY で確認済み。TOLNRD でグリッド設定） |

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
